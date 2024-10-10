package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

object fuseWithLocalMsgSynthRemote {
    def apply[T <: Message, V <: Message, K](part: BSPModel.Partition{type Member = Actor},
            parsePid: T => Int,
            remoteToLocal: T => IndexedSeq[V],
            localToValue: V => K, 
            valuesToRemote: (Long, IndexedSeq[K]) => T): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.map(i => i.id).toSet
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            val outRemoteMessages = MutMap[Long, Buffer[K]]()
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
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer[Message]())
                    m.receivedMessages ++= m.connectedAgentIds.view.filter(x => receivedRemote.get(x).isDefined).map(i => receivedRemote(i))
                    m.run()
                })

                part.members.foreach(m => {
                    m.sendMessages.foreach(i => {
                        if (agentIdx.contains(i._1)) {
                            localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                        } else {
                            val pid = indexedRemote.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                            outRemoteMessages.getOrElseUpdate(pid, Buffer[K]()) ++= i._2.asInstanceOf[Buffer[V]].map(i => localToValue(i))
                        }
                        i._2.clear()
                    })
                })

                outRemoteMessages.foreach(p => {
                    sendMessage(p._1, valuesToRemote(id, p._2.toVector))
                })

                outRemoteMessages.clear()
                1
            }
        }
    }
}