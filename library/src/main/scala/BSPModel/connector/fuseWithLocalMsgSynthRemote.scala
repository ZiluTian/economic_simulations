package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

object fuseWithLocalMsgSynthRemote {
    def apply[T <: Message, V <: Message, K](part: BSPModel.Partition{type Member = Actor},
            parsePid: T => Int,
            remoteToLocal: T => IndexedSeq[V],
            valuesToRemote: (Long, IndexedSeq[K]) => T): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.map(i => i.id).toSet
            // (receiver id, messages)
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            
            // (internal id, partition id)
            val localToRemotePart: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            })
            // (internal id, List(receiver id))
            val localToRemoteIds: Map[Long, Iterable[Long]] = part.members.map(i => {
                (i.id, i.connectedAgentIds.filterNot(j => agentIdx.contains(j)))
            }).toMap

            override def run(): Int = {
                // (internal id, value)
                var outValues = Map[Long, K]()
                // (receiver partition id, messages)
                // (sender id, message)
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = parsePid(i.asInstanceOf[T])
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(remoteToLocal(i.asInstanceOf[T]))
                    }
                }).toMap

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer())
                    m.receivedMessages ++= localToRemoteIds.getOrElse(m.id, List()).view.map(x => receivedRemote.get(x)).filter(_.isDefined).map(_.get)
                    m.run()
                })

                part.members.foreach(m => {
                    m.sendMessages.foreach(i => {
                        if (agentIdx.contains(i._1)) {
                            localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                        } else if (!outValues.isDefinedAt(m.id)){
                            outValues = outValues + (m.id -> i._2.head.value.asInstanceOf[K])
                            // assume sending the same value in all messages
                            // val pid = localToRemotePart.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                            // outRemoteMessages = outRemoteMessages + (pid -> (outRemoteMessages.getOrElse(pid, Vector[K]()) ++ i._2.asInstanceOf[Buffer[V]].map(_.value.asInstanceOf[K])))
                        }
                        i._2.clear()
                    })
                })

                part.topo.outIntVertices.foreach(i => {
                    sendMessage(i._1.toLong, valuesToRemote(id, i._2.map(j => outValues(j.asInstanceOf[Int].toLong))))
                })

                // outRemoteMessages.foreach(p => {
                //     sendMessage(p._1, valuesToRemote(id, p._2))
                // })
                1
            }
        }
    }
}