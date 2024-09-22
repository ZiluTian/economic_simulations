package BSPModel

import scala.collection.mutable.{Map => MutMap, ArrayBuffer}

trait Optimizer[T <: Partition, V <: Partition] extends DebugTrace{
    def transform(part: T): V
}

object Optimize {
    def default(part: Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}): Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId} = {
        StageAndFuseLocalCommunication.transform(
            StageRemoteCommunication.transform(
                RefineCommunication.transform(part)
            )
        )
    }
}

// (BSP, (fixedLocal, fixedRemote))
case object RefineCommunication extends Optimizer[
    Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}, 
    Partition{type Member = (BSP with ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}] {
    
    def transform(part: Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}): Partition{type Member = (BSP with ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId} = {
        new Partition {self => 
            type NodeId = BSPId
            type Member = (BSP with ComputeMethod, (Iterable[BSPId], Iterable[BSPId]))

            dbg(f"Partition vertices in the topo are ${part.topo.vertices}")
            dbg(f"External vertices that are sources of incoming edges are ${part.topo.inExtVertices}")
            dbg(f"Internal vertices that are sources of outgoing edges are ${part.topo.outIntVertices}")

            val id = part.id
            val bspIds = part.members.map(bsp => bsp.asInstanceOf[BSP].id)
            val topo: ArrayGraph[NodeId] = new ArrayGraph[NodeId] {
                    val vertices = part.topo.vertices
                    val edges = part.topo.edges
                    val inExtVertices = part.topo.inExtVertices
                    val outIntVertices = part.topo.outIntVertices.map(i => {
                        (i._1, i._2.map(j => bspIds.indexOf(j)))
                    })
                    val inCache = MutMap[PartitionId, Vector[_ <: Any]]()
                }

            val members: List[Member] = part.members.map(bsp => {
                ddbg(f"${bsp.id} receives messages from ${bsp.receiveFrom.mkString(", ")}")
                assert(part.topo.vertices.contains(bsp.id))
                (bsp, bsp.receiveFrom match {
                    case FixedCommunication(xs) => {
                        xs.partition(i => part.topo.vertices.contains(i))
                    }
                    case HybridCommunication(xs, ys) => {
                        xs.partition(i => part.topo.vertices.contains(i))
                    }
                    case _ => (List(), List())
                })
            })
        }
    }
}

// Cascaded staging
// stage 1: compile away remote messages with cache
// stage 2: compile away local messages and fuse vertices
case object StageRemoteCommunication extends Optimizer[
    Partition{type Member = (BSP with ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}, 
    Partition{type Member = (BSP with ComputeMethod with Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}] {
    
    def transform(part: Partition{type Member = (BSP with ComputeMethod, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}): Partition{type Member = (BSP with ComputeMethod with Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId} = {
        dbg(f"Partitioned neighbors are ${part.members.head._2._1} ${part.members.head._2._2}")
        new Partition {
            type NodeId = BSPId
            type Member = (BSP with ComputeMethod with Stage, (Iterable[BSPId], Iterable[BSPId]))

            val id = part.id
            // pad with inCache
            val topo: ArrayGraph[BSPId] = part.topo.asInstanceOf[ArrayGraph[BSPId]]
            dbg(f"Topo inExtVertices is ${topo.inExtVertices}")

            val members: List[Member] = part.members.map(bspIt => {
                val bsp = bspIt._1
                val remoteIds = bspIt._2._2
                ddbg(f"${bsp.id} receives local messages from ${bspIt._2._1.mkString(", ")} and remote messages from ${remoteIds.mkString(", ")}")

                def genNewBSP(): BSP with ComputeMethod with Stage = 
                    new BSP with ComputeMethod with Stage { selfBSP => 

                        type State = bsp.State
                        type InMessage = bsp.InMessage
                        type OutMessage = bsp.OutMessage
                        
                        var state = bsp.state
                        val id = bsp.id
                        val receiveFrom = bsp.receiveFrom

                        val stagedComputation: List[StagedExpr] = 
                            if (remoteIds.size > 0) {
                                List(new StagedExpr {
                                    type Message = selfBSP.InMessage

                                    // .toVector transforms the map to iterable, to allow duplicate keys
                                    private val receiveRemote: Iterable[(PartitionId, Int)] = 
                                        topo.inExtVertices.map(k => (k._1, k._2.intersect(remoteIds.toVector))).filter(i => !i._2.isEmpty).toVector.flatMap(i => i._2.map(j => (i._1, topo.inExtVertices(i._1).indexOf(j))))

                                    dbg(f"Receive remote is $receiveRemote")
                                    assert(remoteIds.size == receiveRemote.size)
                                        // remoteIds.map(i => topo.asInstanceOf[ArrayGraph[BSPId]].getInboxCacheIndex(i).get)
                                    override def compile(): Option[Message] = {
                                        var tmp: List[bsp.InMessage] = List()
                                        receiveRemote.foreach(i => {
                                            dbg(f"Topo incache has value ${topo.inCache.get(i._1)}")
                                            if (topo.inCache.get(i._1).isDefined && topo.inCache(i._1).size >= i._2) {
                                                tmp = topo.inCache(i._1)(i._2).asInstanceOf[bsp.InMessage] :: tmp
                                            }
                                        })
                                        dbg(f"$id calls partial compute by compiling away received remote!")
                                        partialCompute(tmp)
                                    }
                                    // ddbg("Receive from contains remote values " + receiveRemote)
                                })
                            } else {
                                List()
                            }

                        def partialCompute(ms: Iterable[InMessage]): Option[InMessage] = bsp.partialCompute(ms)
                        def updateState(s: State, m: Option[InMessage]): State = bsp.updateState(s, m)
                        def stateToMessage(s: State): OutMessage = bsp.stateToMessage(s)
                }
                (genNewBSP(), (bspIt._2._1, remoteIds))
            })
        }
    }
}

// Fuse multiple BSPs into one BSP whose state is Array[BSP with ComputeMethod with DoubleBuffer]
// Double buffering is needed to ensure correct semantics
// Lift messages up from vertex-level to partition-level
case object StageAndFuseLocalCommunication extends Optimizer[
    Partition{type Member = (BSP with ComputeMethod with Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}, 
    Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId}]{

        def transform(part: Partition{type Member = (BSP with ComputeMethod with Stage, (Iterable[BSPId], Iterable[BSPId])); type NodeId = BSPId}): Partition{type Member = BSP with ComputeMethod; type NodeId = BSPId} = {
            val bspIds = part.members.map(bsp => bsp._1.asInstanceOf[BSP].id)
            val localIds = part.members.flatMap(bsp => bsp._2._1)

            ddbg("Local ids are " + localIds.mkString(", "))

            new Partition {self =>
                type NodeId = BSPId
                type Member = BSP with ComputeMethod

                val id = part.id
                val topo: ArrayGraph[BSPId] = part.topo.asInstanceOf[ArrayGraph[BSPId]]

                def genNewBSP(bsp: BSP with ComputeMethod with Stage, receiveFromLocal: Iterable[BSPId]): BSP with ComputeMethod with DoubleBuffer with Stage = 
                    new BSP with ComputeMethod with Stage with DoubleBuffer { selfBSP => 

                        type State = bsp.State
                        type InMessage = bsp.InMessage
                        type OutMessage = bsp.OutMessage

                        var state = bsp.state
                        var publicState = bsp.stateToMessage(bsp.state)
                        val id = bsp.id
                        val receiveFrom = bsp.receiveFrom

                        val stagedComputation: List[StagedExpr] = 
                            if (receiveFromLocal.size > 0) {
                                new StagedExpr {
                                    type NodeId = Int
                                    type Message = selfBSP.InMessage

                                    private val receiveLocal: Iterable[Int] = receiveFromLocal.map(i => bspIds.indexOf(i))

                                    // ddbg(selfBSP.id + " receive local values are " + receiveLocal.mkString(" ,"))

                                    override def compile(): Option[Message] = {
                                        dbg(f"$id calls partial compute by compiling away locals!")
                                        // assert(receiveLocal.size > 0)
                                        selfBSP.partialCompute(receiveLocal.map(i => members.head.state.asInstanceOf[(Array[BSP with ComputeMethod with Stage with DoubleBuffer])](i).publicState.asInstanceOf[Message]))
                                    }
                                } :: bsp.stagedComputation
                            } else {
                                bsp.stagedComputation
                            }

                        def partialCompute(ms: Iterable[InMessage]): Option[InMessage] = bsp.partialCompute(ms)
                        def updateState(s: State, m: Option[InMessage]): State = bsp.updateState(s, m)
                        def stateToMessage(s: State): OutMessage = bsp.stateToMessage(s)
                }

                // merged BSP no longer has a publicState for other BSPs
                val mergedBSP = new BSP with ComputeMethod {
                    // padded with cached message results
                    type State = Array[BSP with ComputeMethod with Stage with DoubleBuffer]
                    
                    val id = part.id
                    // partition ids
                    // This restricts fuse operation to each partition
                    // Cannot fuse different parts of a partition separately
                    val receiveFrom = part.topo.inExtVertices.keySet
                    type InMessage = Vector[Double]
                    type OutMessage = Vector[Double]

                    var state: State = (part.members.map(b => genNewBSP(b._1.asInstanceOf[BSP with ComputeMethod with Stage], b._2._1)).toArray.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]])

                    // simply update the message component of the state
                    // executing BSPs in the array is done in in-place run
                    // leave it to runtime
                    def updateState(s: State, m: Option[InMessage]): State = ???

                    // Need to generate different messages for different partitions. Leave it to runtime
                    def stateToMessage(s: State): OutMessage = ???
                    def partialCompute(ms: Iterable[InMessage]): Option[InMessage] = ???

                    // in-place update to each BSP inside
                    override def run(ms: Iterable[InMessage]): Unit = {
                        state.foreach(bsp => {
                            bsp.run(List())
                        })
                        state.foreach(_.updatePublicState())
                    }

                    override def toString(): String = {
                        state.map(_.state).mkString(", ")
                    }
                }

                val members = List(mergedBSP)
            }
        }
}