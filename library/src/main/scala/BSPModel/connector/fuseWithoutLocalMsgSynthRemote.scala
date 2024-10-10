package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

// Actors need to be generated with noMessage
object fuseWithoutLocalMsgSynthRemote {
    def apply[T <: Message, V <: Message, K](part: BSPModel.Partition{type Member = Actor},
            parsePid: T => Int,
            remoteToLocal: T => IndexedSeq[V],
            localToRemote: (Long, Vector[K]) => T): Actor = {
        new Actor {
            id = part.id.toLong
            val localReceivedMessages = MutMap[Long, Message]()
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            }).toMap

            override def run(): Int = {
                val receivedRemote: Map[Long, Message] = receivedMessages.flatMap(i => {
                    val pid = parsePid(i.asInstanceOf[T])
                    part.topo.inExtVertices.get(pid) match {
                        case None => throw new Exception(f"Receive a remote message from unknown $pid")
                        case Some(x) => x.asInstanceOf[Vector[Int]].view.map(_.toLong).zip(remoteToLocal(i.asInstanceOf[T]))
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
                })

                part.members.foreach(m => {
                    localReceivedMessages(m.id) = m.sendMessages.getOrElse(m.id, throw new Exception(f"Double buffered message not found for ${m.id} in part $id")).head
                    m.sendMessages.clear()
                })

                part.topo.outIntVertices.foreach(i => {
                    sendMessage(i._1.toLong, localToRemote(id, i._2.map(j => localReceivedMessages(j.asInstanceOf[Int].toLong).value.asInstanceOf[K])))
                })
                1
            }
        }
    }
}