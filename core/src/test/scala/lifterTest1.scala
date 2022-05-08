package meta.test

import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import meta.deep.IR.TopLevel.ClassWithObject
import meta.deep.IR
import meta.runtime.{Actor}
import meta.API._
import org.scalatest.FlatSpec
import scala.util.Random
import meta.runtime.Future
import scala.collection.mutable.ListBuffer

@lift
class AgentWithBlockingCall(val n: AgentWithBlockingCall) extends Actor {

    def blockingMtd(): Boolean = {
        println(id + " processes blocking mtd!")
        waitLabel(Turn, 1)
        println(id + " finishes processing!")
        true
    }

    def sendAsyncMsg(): Boolean = {
        List(n).foreach(i => asyncMessage(() => i.blockingMtd()))
        Set(n).foreach(i => asyncMessage(() => i.blockingMtd()))
        Map(1 -> n).map(i => i._2).foreach(i => asyncMessage(() => i.blockingMtd()))
        ListBuffer(1, 2, 3, 4).foreach(j => asyncMessage(() => n.blockingMtd()))
        true
    }

    def main(): Unit = {
        while (true){
            println("Inside agent "+id)
            if (n != null){
                n.blockingMtd()
            }
            waitAndReply(1)
        }
    }
}


class lifterTest1 extends FlatSpec with org.scalatest.Matchers {
    import meta.deep.IR.Predef._
    import meta.classLifting.Lifter

    "Lift a blocking method" should "compile" in {
        val liftMyClass2: ClassWithObject[AgentWithBlockingCall] = AgentWithBlockingCall.reflect(IR)
        val liftedRes = new Lifter().apply(List(liftMyClass2)) 
        liftedRes._1.head.methods.foreach(x => {
            assert(!x.body.toString.contains("SpecialInstructions"))
        })

        val liftedMainClass = liftedRes._1.head.main
        assert(!liftedMainClass.toString.contains("SpecialInstructions"))
    }
}