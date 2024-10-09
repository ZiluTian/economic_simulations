package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import BSPModel.example.gameOfLife._
import scala.util.Random
import meta.runtime.{IntVectorMessage, IntArrayMessage, Actor}
import BSPModel.Connector._
import BSPModel.example.stockMarket._
import BSPModel.example.epidemics._

abstract class EpidemicsUnoptTest extends scaleUpTest {
    override val totalRounds: Int = 50

    def genPopulation(g: Map[Int, Iterable[Int]]): IndexedSeq[Actor] = {
        g.map(i => {
            val age: Int = Random.nextInt(90)+10
            bspToAgent(new PersonAgent(i._1, 
                    age, 
                    i._2.toVector, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0))
        }).toVector
    }
}

class gameOfLifeUnoptTest extends scaleUpTest {

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val height: Int = scaleUpFactor*(baseFactor / width).toInt
        val graph = GraphFactory.torus2D(width, height)
        graph.adjacencyList().map(i => bspToAgent(new Cell(i._1.toInt, i._2.map(_.toInt).toSeq))).toVector        
    }
}

class stockMarketUnoptTest extends scaleUpTest {
    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val markets: Int = 1
        val traders: Int = baseFactor * scaleUpFactor - 1
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001
        val graph = GraphFactory.bipartite(markets, traders)
        val cells: Map[Int, BSP with ComputeMethod] = 
            ((0 until markets).map(i => {
                (i, new MarketAgent(i, (markets until markets + traders), initialStockPrice))
            }) ++ (0 until traders).map(i => {
                (i + markets, new TraderAgent(markets + i, (0 until markets), budget, interestRate))
            })).toMap
        cells.values.map(i => bspToAgent(i)).toVector
    }
}

class ERMUnoptTest extends EpidemicsUnoptTest {
    val p = 0.01

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)
        genPopulation(toGraphInt(graph.adjacencyList()))
    }
}

class SBMUnoptTest extends EpidemicsUnoptTest {
    val p =0.01
    val q = 0
    val numBlocks = 5

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val graph = GraphFactory.stochasticBlock(baseFactor * scaleUpFactor, p, q, numBlocks)
        genPopulation(toGraphInt(graph.adjacencyList()))
    }
}