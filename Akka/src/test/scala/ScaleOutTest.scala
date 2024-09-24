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

// 10k per machine
abstract class scaleOutTest extends FlatSpec {
    val baseFactor: Int = 10000
    val totalRounds: Int = 200
    val repeat: Int = 3
    val expNameWithDollar: String = getClass.getSimpleName
    val expName: String = expNameWithDollar.substring(0, expNameWithDollar.length -1)
    lazy val file = new File(f"scaleOut$baseFactor/$expName.log")
    
    def forceGC(): Unit = {
        System.gc()
        System.runFinalization()
        Thread.sleep(100)  // Give GC time to complete
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor]

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

object gameOfLifeScaleOutTest extends scaleOutTest with App {
    val width: Int = 100

    override def main(args: Array[String]): Unit = {
        exec(args)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(baseFactor % totalMachines == 0)
        val partSize: Int = baseFactor / totalMachines
        val height: Int = baseFactor / width

        (0L until partSize).map(i => {
            val idx = partSize * machineId + i
            val cell = if (Random.nextBoolean) {
                new generated.example.gameOfLife.Cell(1)
            } else {
                new generated.example.gameOfLife.Cell(0)
            }
            cell.id = idx
            val x: Long = idx % width
            val y: Long = idx / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + height) % height
            } yield dy * width + dx
            cell.connectedAgentIds = neighbors
            cell
        })
    }
}

object stockMarketScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(baseFactor % totalMachines == 0)
        val partSize: Int = baseFactor / totalMachines
        if (machineId == 0) {
            val market = new generated.example.stockMarket.v2.Market()
            market.id = 0
            val traders = (1 to partSize).map(i => {
                val trader = new generated.example.stockMarket.v2.Trader(1000, 0.001)
                trader.id = i
                trader
            })
            market.connectedAgentIds = (1L to baseFactor)
            Vector(market) ++ traders
        } else {
            ((partSize * machineId + 1) to (partSize * (machineId + 1))).map(i => {
                val trader = new generated.example.stockMarket.v2.Trader(1000, 0.001)
                trader.id = i
                trader
            })
        }
    }
}

object ERMScaleOutTest extends scaleOutTest with App {
    override val totalRounds: Int = 50
    val p: Double = 0.01

    override def main(args: Array[String]): Unit = {
        exec(args)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(baseFactor % totalMachines == 0)
        val partSize: Int = baseFactor / totalMachines

        (0L until partSize).map(i => {
            val idx = partSize * machineId + i
            val cell = new generated.example.epidemic.v2.Person(Random.nextInt(90) + 10)
            cell.id = idx
            cell.connectedAgentIds = (0L until baseFactor).filter(j => (Random.nextDouble() < p) && (j != idx))
            cell
        })
    }
}

object SBMScaleOutTest extends scaleOutTest with App {
    override val totalRounds: Int = 50
    val p: Double = 0.01
    val q: Double = 0 

    override def main(args: Array[String]): Unit = {
        exec(args)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(baseFactor % totalMachines == 0)
        val partSize: Long = baseFactor / totalMachines
        (0L until partSize).map(i => {
            val idx: Long = partSize * machineId + i
            val cell = new generated.example.epidemic.v2.Person(Random.nextInt(90) + 10)
            cell.id = idx
            // the number of blocks is total machines. Only connect with neighbors in the same partition
            cell.connectedAgentIds = (partSize * machineId until partSize * (machineId + 1)).filter(j => (Random.nextDouble() < p) && (j != idx))
            cell
        })
    }
}