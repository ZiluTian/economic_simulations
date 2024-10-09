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

abstract class EpidemicsFusionNoLocalMessageTest extends scaleUpTest {
    override val totalRounds: Int = 50

    def genPopulation(g: Map[Int, Iterable[Int]]): Map[Int, Actor] = {
        g.map(i => {
            val age: Int = Random.nextInt(90)+10
            (i._1, bspToAgent.noMessage(new PersonAgent(i._1, 
                    age, 
                    i._2.toVector, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0)))
        }).toMap
    }
}

class gameOfLifeFusionNoLocalMessageTest extends scaleUpTest {

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val height: Int = scaleUpFactor*(baseFactor / width).toInt

        val graph = GraphFactory.torus2D(width, height)
        val cells: Map[Int, Actor] = toGraphInt(graph.adjacencyList()).map(i => (i._1, bspToAgent.noMessage(new Cell(i._1, i._2.toSeq)))).toMap
        (0 until scaleUpFactor).map(i => {
            val tp = partition2DArray(i, scaleUpFactor, width, (baseFactor / width).toInt)
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = i
                val topo = tp
                val members = tp.vertices.map(j => cells(j)).toList
            }
            partToAgent.fuseWithoutLocalMsgIntVectorAgent(part)
        }).toVector
    }
}

class stockMarketFusionNoLocalMessageTest extends scaleUpTest {
    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val markets: Int = 1
        val traders: Int = baseFactor * scaleUpFactor - 1
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001
        val graph = GraphFactory.bipartite(markets, traders)
        val cells: Map[Int, Actor] = 
            ((0 until markets).map(i => {
                (i, bspToAgent.noMessage(new MarketAgent(i, (markets until markets + traders), initialStockPrice)))
            }) ++ (0 until traders).map(i => {
                (i + markets, bspToAgent.noMessage(new TraderAgent(markets + i, (0 until markets), budget, interestRate)))
            })).toMap
        partition(graph, scaleUpFactor).zipWithIndex.par.map(i => {
            // println(f"part ${i._2} vertices are ${i._1.vertices}")
            // println(f"part ${i._2}, in edges ${i._1.inExtVertices}")
            // println(f"part ${i._2}, out edges ${i._1.outIntVertices}")

            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = i._2
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            partToAgent.fuseWithoutLocalMsgDoubleVectorVectorAgent(part)
        }).seq.toVector
    }
}

class ERMFusionNoLocalMessageTest extends EpidemicsFusionNoLocalMessageTest {
    val p = 0.01

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)
        val cells: Map[Int, Actor] = genPopulation(toGraphInt(graph.adjacencyList()))
        partition(graph, scaleUpFactor).zipWithIndex.par.map(i => {
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = i._2
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            partToAgent.fuseWithoutLocalMsgDoubleVectorAgent(part)
        }).seq.toVector
    }
}

class SBMFusionNoLocalMessageTest extends EpidemicsFusionNoLocalMessageTest {
    val p =0.01
    val q = 0
    val numBlocks = 5

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val graph = GraphFactory.stochasticBlock(baseFactor * scaleUpFactor, p, q, numBlocks)
        val cells: Map[Int, Actor] = genPopulation(toGraphInt(graph.adjacencyList()))
        partition(graph, scaleUpFactor).zipWithIndex.par.map(i => {
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = i._2
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            partToAgent.fuseWithoutLocalMsgDoubleVectorAgent(part)
        }).seq.toVector
    }
}