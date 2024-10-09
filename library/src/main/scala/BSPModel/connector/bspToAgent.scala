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
}

object partToAgent {
    import bspToAgent.{toOutMessage}
    
    def fuseWithLocalMsgIntVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[IntMessage]]()

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[IntVectorMessage].value(0).toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].map(_.toLong).zip(i.asInstanceOf[IntVectorMessage].value.tail.map(j => IntMessage(j)))
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
                                var valid = false
                                part.topo.outIntVertices.foreach(j => {
                                    if (j._2.contains(m.id)) {
                                        outRemoteMessages.getOrElseUpdate(j._1, Buffer[IntMessage]()) ++= i._2.asInstanceOf[Buffer[IntMessage]]
                                        valid = true
                                    }
                                })
                                if (!valid){
                                    throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!")
                                }
                        }
                        i._2.clear()
                    })
                    
                    outRemoteMessages.foreach(p => {
                        sendMessage(p._1, IntVectorMessage(id.toInt +: p._2.map(_.asInstanceOf[IntMessage].value).toVector))
                    })

                    outRemoteMessages.clear()
                })
                1
            }
        }
    }

    def fuseWithLocalMsgDoubleVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[DoubleMessage]]()

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[DoubleVectorMessage].value(0).toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].map(_.toLong).zip(i.asInstanceOf[DoubleVectorMessage].value.tail.map(j => DoubleMessage(j)))
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
                                var valid = false
                                part.topo.outIntVertices.foreach(j => {
                                    if (j._2.contains(m.id)) {
                                        outRemoteMessages.getOrElseUpdate(j._1, Buffer[DoubleMessage]()) ++= i._2.asInstanceOf[Buffer[DoubleMessage]]
                                        valid = true
                                    }
                                })
                                if (!valid){
                                    throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!")
                                }
                        }
                        i._2.clear()
                    })
                    
                    outRemoteMessages.foreach(p => {
                        sendMessage(p._1, DoubleVectorMessage(id.toDouble +: p._2.map(_.asInstanceOf[DoubleMessage].value).toVector))
                    })

                    outRemoteMessages.clear()
                })
                1
            }
        }
    }

    def fuseWithLocalMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.zipWithIndex.map(i => (i._1.id, i._2)).toMap
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[DoubleVectorMessage]]()

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = i.asInstanceOf[DoubleVectorVectorMessage].value(0).head.toInt
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].map(_.toLong).zip(i.asInstanceOf[DoubleVectorVectorMessage].value.tail.map(j => DoubleVectorMessage(j)))
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
                                var valid = false
                                part.topo.outIntVertices.foreach(j => {
                                    if (j._2.contains(m.id)) {
                                        outRemoteMessages.getOrElseUpdate(j._1, Buffer[DoubleVectorMessage]()) ++= i._2.asInstanceOf[Buffer[DoubleVectorMessage]]
                                        valid = true
                                    }
                                })
                                if (!valid){
                                    throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!")
                                }
                        }
                        i._2.clear()
                    })
                    
                    outRemoteMessages.foreach(p => {
                        sendMessage(p._1, DoubleVectorVectorMessage(Vector(id.toDouble) +: p._2.map(_.asInstanceOf[DoubleVectorMessage].value).toVector))
                    })

                    outRemoteMessages.clear()
                })
                1
            }
        }
    }
}