package BSPModel

import meta.runtime._
import scala.collection.mutable.{Map => MutMap, Buffer}

object bspToAgent{
    def toOutMessage[T](x: T): meta.runtime.Message = {
        x match {
            case x: Int => IntMessage(x)
            case x: Double => DoubleMessage(x)
            case x: Vector[Double] => DoubleVectorMessage(x)
        }
    }

    def toInMessage[T](x: meta.runtime.Message): T = {
        x.value.asInstanceOf[T]
    }

    def apply(bsp: BSP with ComputeMethod): meta.runtime.Actor = {
        new meta.runtime.Actor {
            id = bsp.id 
            override def run(): Int = {
                // println(f"$id receives ${receivedMessages.size} messages")
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
    def fuseWithRemoteMsgIntVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithRemoteMsg(part, 
            (i: IntVectorMessage) => (i.value.head.toLong, IntMessage(i.asInstanceOf[IntVectorMessage].value(1))),
            (i: Long, j: IntMessage) => IntVectorMessage(Vector(i.toInt, j.value))
        )
    }

    def fuseWithRemoteMsgDoubleVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithRemoteMsg(part, 
            (i: DoubleVectorMessage) => (i.value.head.toLong, DoubleMessage(i.asInstanceOf[DoubleVectorMessage].value(1))),
            (i: Long, j: DoubleMessage) => DoubleVectorMessage(Vector(i.toDouble, j.value))
        )
    }

    def fuseWithRemoteMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithRemoteMsg(part, 
            (i: DoubleVectorVectorMessage) => (i.value.head.head.toLong, DoubleVectorMessage(i.asInstanceOf[DoubleVectorVectorMessage].value(1))),
            (i: Long, j: DoubleVectorMessage) => DoubleVectorVectorMessage(Vector(i.toDouble) +: Vector(j.value))
        )
    }

    def fuseWithLocalMsgIntVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithLocalMsgSynthRemote(part,
            (i: IntVectorMessage) => i.value.head,
            (i: IntVectorMessage) => i.value.tail.map(j => IntMessage(j)),
            (i: IntMessage) => i.value,
            (id: Long, p: IndexedSeq[Int]) => IntVectorMessage(id.toInt +: p.toVector)
        )
    }

    def fuseWithLocalMsgDoubleVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithLocalMsgSynthRemote(part,
            (i: DoubleVectorMessage) => i.value.head.toInt,
            (i: DoubleVectorMessage) => i.value.tail.map(j => DoubleMessage(j)),
            (i: DoubleMessage) => i.value,
            (id: Long, p: IndexedSeq[Double]) => DoubleVectorMessage(id.toDouble +: p.toVector)
        )
    }

    def fuseWithLocalMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithLocalMsgSynthRemote(part,
            (i: DoubleVectorVectorMessage) => i.value.head.head.toInt,
            (i: DoubleVectorVectorMessage) => i.value.tail.map(j => DoubleVectorMessage(j)),
            (i: DoubleVectorMessage) => i.value,
            (id: Long, p: IndexedSeq[Vector[Double]]) => DoubleVectorVectorMessage(Vector(id.toDouble) +: p.toVector)
        )
    }

    def fuseWithoutLocalMsgIntVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithoutLocalMsgSynthRemote(part,
            (i: IntVectorMessage) => i.value.head,
            (i: IntVectorMessage) => i.value.tail.map(j => IntMessage(j)),
            (id: Long, p: Vector[Int]) => IntVectorMessage(id.toInt +: p)
        )
    }

    def fuseWithoutLocalMsgDoubleVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
       fuseWithoutLocalMsgSynthRemote(part,
            (i: DoubleVectorMessage) => i.value.head.toInt,
            (i: DoubleVectorMessage) => i.value.tail.map(j => DoubleMessage(j)),
            (id: Long, p: Vector[Double]) => DoubleVectorMessage(id.toDouble +: p)
        )
    }

    def fuseWithoutLocalMsgDoubleVectorVectorAgent(part: BSPModel.Partition{type Member = Actor}): Actor = {
        fuseWithoutLocalMsgSynthRemote(part,
            (i: DoubleVectorVectorMessage) => i.value.head.head.toInt,
            (i: DoubleVectorVectorMessage) => i.value.tail.map(j => DoubleVectorMessage(j)),
            (id: Long, p: Vector[Vector[Double]]) => DoubleVectorVectorMessage(Vector(id.toDouble) +: p)
        )
    }
}