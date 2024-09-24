package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{IntVectorMessage, IntArrayMessage, Actor}
import java.io.{File, PrintStream}

// 1k per component. Scale up to 100k
abstract class scaleOutTest extends FlatSpec {
    val baseFactor: Int = 1000
    val totalRounds: Int = 20
    val repeat: Int = 1
    val expNameWithDollar: String = getClass.getSimpleName
    val expName: String = expNameWithDollar.substring(0, expNameWithDollar.length -1)
    lazy val file = new File(f"scaleOut$baseFactor/$expName.log")
    
    def forceGC(): Unit = {
        System.gc()
        System.runFinalization()
        Thread.sleep(100)  // Give GC time to complete
    }

    def gen(partId: Int, totalParts: Int): IndexedSeq[Actor]

    def startDriver(ip: String, port: Int, totalMachines: Int): Unit = {
        val printStream = new PrintStream(file)
        System.setOut(printStream)
        val conf = Map("role" -> "Driver", "ip" -> ip, "port" -> port, "name" -> expName, "data" -> "snapshot", "totalMachines" -> totalMachines)            
        val ts = API.Simulate(Vector[Actor](), totalRounds, conf)(DeforestationStrategy.NoReduction)
        forceGC()
        printStream.close()
    }

    def startWorker(ip: String, port: Int, totalMachines: Int, machineId: Int, seed: String): Unit = {
        val agents = gen(machineId, totalMachines)
        val conf = Map("role" -> f"Machine-${machineId}", "ip" -> ip, "port" -> port, "seed" -> seed, "name" -> expName, "data" -> "snapshot", "totalMachines" -> totalMachines)            
        val ts = API.Simulate(agents, totalRounds, conf)(DeforestationStrategy.NoReduction)
    }

    def exec(args: Array[String]): Unit = {
        assert(args.size >= 4)
        val role: String = args(0)
        val ip: String = args(1)
        val port: Int = args(2).toInt
        val totalMachines: Int = args(3).toInt

        if (role == "driver") {
            startDriver(ip, port, totalMachines)
        } else {
            assert(args.size==6)
            val machineId: Int = args(4).toInt
            val seed: String = args(5)
            startWorker(ip, port, totalMachines, machineId, seed)
        }
    }
}

// class gameOfLifeTest extends scaleOutTest {
//     val width: Int = 100
//     def gen(x: Int): IndexedSeq[Actor] = {
//         generated.example.gameOfLife.InitData(width, x*(baseFactor/width).toInt)
//     }
// }

object stockMarketScaleoutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args)
    }

    def gen(partId: Int, totalParts: Int): IndexedSeq[Actor] = {
        assert(baseFactor % totalParts == 0)
        val partSize: Int = baseFactor / totalParts
        if (partId == 0) {
            val market = new generated.example.stockMarket.v2.Market()
            market.id = 0
            val traders = (1 to partSize).map(i => {
                val trader = new generated.example.stockMarket.v2.Trader(1000, 0.001)
                trader.id = i
                trader
            })
            market.connectedAgentIds = (1 to baseFactor).map(_.toLong)
            Vector(market) ++ traders
        } else {
            ((partSize * partId + 1) to (partSize * (partId + 1))).map(i => {
                val trader = new generated.example.stockMarket.v2.Trader(1000, 0.001)
                trader.id = i
                trader
            })
        }
    }
}

// class ERMTest extends scaleUpTest {
//     override val totalRounds: Int = 50

//     def gen(x: Int): IndexedSeq[Actor] = {
//         val graph = cloudcity.lib.Graph.GraphFactory.erdosRenyi(baseFactor * x, 0.01)
//         generated.example.epidemic.v2.InitData(graph)
//     }
// }

// class SBMTest extends scaleUpTest {
//     override val totalRounds: Int = 50

//     def gen(x: Int): IndexedSeq[Actor] = {
//         val graph = cloudcity.lib.Graph.GraphFactory.stochasticBlock(baseFactor * x, 0.01, 0, 5)
//         generated.example.epidemic.v2.InitData(graph)
//     }
// }