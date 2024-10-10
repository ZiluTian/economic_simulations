package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

object fuseWithRemoteMsg {
    def apply[T <: Message, V <: Message](part: BSPModel.Partition{type Member = Actor}, 
            parseRemote: T => (Long, V), 
            localToRemote: (Long, V) => T): Actor = {
        new Actor {
            id = part.id.toLong
            val agentIdx = part.members.map(i => i.id).toSet
            // (receiver id, messages)
            val localReceivedMessages = MutMap[Long, Buffer[Message]]()
            // (internal id, external partition id)
            val indexedRemote: Map[Long, Long] = part.topo.outIntVertices.flatMap(i => {
                i._2.map(k => (k.asInstanceOf[Int].toLong, i._1.toLong))
            })

            override def run(): Int = {
                // (receiver id, messages)
                val receivedRemote: Map[Long, Buffer[Message]] = receivedMessages.map(i => {
                    parseRemote(i.asInstanceOf[T])
                }).groupBy(_._1).mapValues(j => j.map(_._2))
                // println(f"Round $time $id received remote messages ${receivedRemote}")

                receivedMessages.clear()
                
                part.members.foreach(m => {
                    m.receivedMessages = localReceivedMessages.remove(m.id).getOrElse(Buffer[Message]())
                    m.receivedMessages ++= receivedRemote.getOrElse(m.id, List())
                    m.run()
                })

                part.members.foreach(m => {    
                    m.sendMessages.foreach(i => {
                        if (agentIdx.contains(i._1)) {
                            // println(f"${m.id} sends a local message to ${i}")
                            localReceivedMessages.getOrElseUpdate(i._1, Buffer[Message]()) ++= i._2
                        } else {
                            // println(f"${m.id} sends a remote message to ${i}")
                            val pid = indexedRemote.getOrElse(m.id, throw new Exception(f"${m.id} in ${id} attempts to send a message to ${i._1}, which is not local or in ${part.topo.outIntVertices}!"))
                            i._2.foreach(j => {
                                sendMessage(pid, localToRemote(i._1, j.asInstanceOf[V]))
                            })
                        }
                        i._2.clear()
                    })
                })

                // println(f"$id Locally received messages ${localReceivedMessages}")
                // println(f"$id Messages in the mailbox are ${sendMessages}")

                1
            }
        }
    }
}