package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

object bspToAgent{
    def toOutMessage[T](x: T): meta.runtime.Message = {
        x match {
            case x: Int => new IntMessage(x)
            case x: Double => new DoubleMessage(x)
            case x: Vector[Double] => new DoubleVectorMessage(x)
        }
    }

    def toInMessage[T](x: meta.runtime.Message): T = {
        x.value.asInstanceOf[T]
    }

    def apply(bsp: BSP with ComputeMethod): meta.runtime.Actor = {
        new meta.runtime.Actor {
            id = bsp.id 
            override def run(): Int = {
                bsp.run(receivedMessages.map(i => bsp.deserialize(toInMessage[bsp.SerializeFormat](i))))
                receivedMessages.clear()
                val outMessage = toOutMessage(bsp.message())
                bsp.receiveFrom.foreach(n => {
                    sendMessage(n, outMessage)
                })
                1
            }
        }
    }

    // only add one message to make its value available 
    def noMessage(bsp: BSP with ComputeMethod): meta.runtime.Actor = {
        new meta.runtime.Actor {
            id = bsp.id 
            connectedAgentIds = bsp.receiveFrom.map(_.toLong)
            override def run(): Int = {
                bsp.run(receivedMessages.map(i => bsp.deserialize(toInMessage[bsp.SerializeFormat](i))))
                receivedMessages.clear()
                val outMessage = toOutMessage(bsp.message())
                sendMessage(id, outMessage)
                1
            }
        }
    }
}

object partToAgent {
    import bspToAgent.{toOutMessage}
    
    def fuseWithLocalMsgIntVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[Int]]()
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            }).toMap

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[IntVectorMessage].value(0).toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(i.asInstanceOf[IntVectorMessage].value.tail.map(j => IntMessage(j)))
                    }
                }).toMap

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer[Message]())
                    m.receivedMessages ++= m.connectedAgentIds.view.filter(x => receivedRemote.get(x).isDefined).map(i => receivedRemote(i))
                    m.run()
                    m.sendMessages.foreach(i => {
                        agentIdx.get(i._1) match {
                            case Some(k) => 
                                localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                            case None => 
                                val pid = indexedRemote.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                                outRemoteMessages.getOrElseUpdate(pid, Buffer[Int]()) ++= i._2.asInstanceOf[Buffer[IntMessage]].map(_.value)
                        }
                        i._2.clear()
                    })
                })

                outRemoteMessages.foreach(p => {
                    sendMessage(p._1, IntVectorMessage(id.toInt +: p._2.toVector))
                })

                outRemoteMessages.clear()
                1
            }
        }
    }

    def fuseWithLocalMsgDoubleVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[Double]]()
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            }).toMap

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[DoubleVectorMessage].value(0).toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(i.asInstanceOf[DoubleVectorMessage].value.tail.map(j => DoubleMessage(j)))
                    }
                }).toMap

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer[Message]())
                    m.receivedMessages ++= m.connectedAgentIds.view.filter(x => receivedRemote.get(x).isDefined).map(i => receivedRemote(i))
                    m.run()
                    m.sendMessages.foreach(i => {
                        agentIdx.get(i._1) match {
                            case Some(k) => 
                                localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                            case None => 
                                val pid = indexedRemote.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                                outRemoteMessages.getOrElseUpdate(pid, Buffer[Double]()) ++= i._2.asInstanceOf[Buffer[DoubleMessage]].map(_.value)
                        }
                        i._2.clear()
                    })
                })

                outRemoteMessages.foreach(p => {
                    sendMessage(p._1, DoubleVectorMessage(id.toDouble +: p._2.toVector))
                })

                outRemoteMessages.clear()
                1
            }
        }
    }

    def fuseWithLocalMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[Vector[Double]]]()
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            }).toMap

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[DoubleVectorVectorMessage].value(0).head.toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(i.asInstanceOf[DoubleVectorVectorMessage].value.tail.map(j => DoubleVectorMessage(j)))
                    }
                }).toMap

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer[Message]())
                    m.receivedMessages ++= m.connectedAgentIds.view.filter(x => receivedRemote.get(x).isDefined).map(i => receivedRemote(i))
                    m.run()
                    m.sendMessages.foreach(i => {
                        agentIdx.get(i._1) match {
                            case Some(k) => 
                                localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                            case None => 
                                val pid = indexedRemote.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                                outRemoteMessages.getOrElseUpdate(pid, Buffer[Vector[Double]]()) ++= i._2.asInstanceOf[Buffer[DoubleVectorMessage]].map(_.value)
                        }
                        i._2.clear()
                    })
                })

                outRemoteMessages.foreach(p => {
                    sendMessage(p._1, DoubleVectorVectorMessage(Vector(id.toDouble) +: p._2.toVector))
                })

                outRemoteMessages.clear()
                1
            }
        }
    }

    def fuseWithoutLocalMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val localReceivedMessages = MutMap[Long, Message]()
            val outRemoteMessages = MutMap[Long, Buffer[Vector[Double]]]()
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            }).toMap

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[DoubleVectorVectorMessage].value(0).head.toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(i.asInstanceOf[DoubleVectorVectorMessage].value.tail.map(j => DoubleVectorMessage(j)))
                    }
                }).toMap

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages.clear()
                    m.connectedAgentIds.foreach(k => {
                        localReceivedMessages.get(k) match {
                            case None => {
                                if (receivedRemote.isDefinedAt(k)) {
                                    m.receivedMessages += receivedRemote(k)
                                }
                            }
                            case Some(v) => m.receivedMessages += v
                        }
                    })
                    m.run()
                    localReceivedMessages(m.id) = m.sendMessages.getOrElse(m.id, throw new Exception(f"Double buffered message not found for ${m.id} in part $id")).head
                    m.sendMessages.clear()
                })

                part.topo.outIntVertices.foreach(i => {
                    sendMessage(i._1.toLong,  DoubleVectorVectorMessage(Vector(id.toDouble) +: i._2.view.map(j => localReceivedMessages(j.asInstanceOf[Int].toLong).value.asInstanceOf[Vector[Double]]).toVector))
                })
                1
            }
        }
    }
}