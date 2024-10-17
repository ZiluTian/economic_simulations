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
abstract class scaleUpTest extends FlatSpec {
    val baseFactor: Int = 1000
    val totalRounds: Int = 200
    val repeat: Int = 3
    val expName: String = getClass.getSimpleName
    lazy val file = new File(f"${expName}.log")
    
    def forceGC(): Unit = {
        System.gc()
        System.runFinalization()
        Thread.sleep(100)  // Give GC time to complete
    }

    def gen(x: Int): IndexedSeq[Actor]
    def bench(): Unit = {
        val printStream = new PrintStream(file)
        System.setOut(printStream)
        
        List(1, 5, 10, 50, 100).foreach(component => {
            (1 to repeat).foreach(i => {
                val agents = gen(component)
                val conf = Map("role" -> "Standalone", "port" -> 8010, "name" -> expName, "data" -> "snapshot", "workersPerMachine" -> component)            
                val ts = API.Simulate(agents, totalRounds, conf)(DeforestationStrategy.NoReduction)
                forceGC()
            })
        })
        printStream.close()
    }

    def overhead(): Unit = {
        val printStream = new PrintStream(new File(f"${expName}_breakdown.log"))
        System.setOut(printStream)
        
        List(1, 5, 10, 50, 100).foreach(component => {
            (1 to repeat).foreach(i => {
                gen(component)
            })
        })
        printStream.close()
    }

    expName should "measure overhead" in {
        overhead()
    }

    expName should "execute" in {
        bench()
    }
}

class gameOfLifeTest extends scaleUpTest {
    val width: Int = 100
    def gen(x: Int): IndexedSeq[Actor] = {
        generated.example.gameOfLife.InitData(width, x*(baseFactor/width).toInt)
    }
}

class stockMarketTest extends scaleUpTest {
    def gen(x: Int): IndexedSeq[Actor] = {
        generated.example.stockMarket.v2.InitData(1, baseFactor*x-1)
    }
}

class ERMTest extends scaleUpTest {
    override val totalRounds: Int = 50

    def gen(x: Int): IndexedSeq[Actor] = {
        val graph = cloudcity.lib.Graph.GraphFactory.erdosRenyi(baseFactor * x, 0.01)
        generated.example.epidemic.v2.InitData(graph)
    }
}

class SBMTest extends scaleUpTest {
    override val totalRounds: Int = 50

    def gen(x: Int): IndexedSeq[Actor] = {
        val graph = cloudcity.lib.Graph.GraphFactory.stochasticBlock(baseFactor * x, 0.01, 0, 5)
        generated.example.epidemic.v2.InitData(graph)
    }
}