package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{DoubleVectorMessage, IntArrayMessage, Actor}
import BSPModel.example.epidemics._
import BSPModel.Connector._

abstract class EpidemicsFusedScaleOutTest extends scaleOutTest {

    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]

        override def run(): Int = {
            receivedMessages.foreach(i => {
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[DoubleVectorMessage].value(0).toInt) = i.asInstanceOf[DoubleVectorMessage].value.tail
            })
            receivedMessages.clear()
            
            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, DoubleVectorMessage(part.id.toDouble +: i._2.map(j => fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState.asInstanceOf[Double])))
            })
            1
        }
    }

    def genPopulation(g: Map[Int, Iterable[Int]]): Map[Int, BSP with ComputeMethod] = {
        g.map(i => {
            val age: Int = Random.nextInt(90)+10
            (i._1, new PersonAgent(i._1, 
                    age, 
                    i._2.toVector, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0))
        })
    }
}

object ERMFusedScaleOutTest extends EpidemicsFusedScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val startingIndex = machineId * baseFactor
        // val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)

        // Generate a partial graph
        val adjList = (startingIndex until (startingIndex + baseFactor)).map { i =>
            val neighbors = (0 until (startingIndex + baseFactor * totalMachines)).filter(j => (i != j) && (Random.nextDouble() < p))
            (i -> neighbors)
        }.toMap

        val cells: Map[Int, BSP with ComputeMethod] = genPopulation(adjList)

        partitionPartialGraph(adjList.toIterable.flatMap(i => i._2.map(j => (j, i._1))), adjList.keys.toSet, (0 until totalMachines * baseFactor).map(i => (i, i % baseFactor)).toMap).view.zipWithIndex.map(i => {
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = i._2
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            BSPModel.Optimize.default(part)
        }).map(i => new partActor(i)).toVector
    }
}

object SBMFusedScaleOutTest extends EpidemicsFusedScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val q: Double = 0
        val numBlocks: Int = 5
        
        val startingIndex = machineId * baseFactor
        // val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)

        // Generate a partial graph
        val adjList = (startingIndex until (startingIndex + baseFactor)).map { i =>
            val neighbors = (startingIndex until (startingIndex + baseFactor)).filter(j => (j != i) && (Random.nextDouble() < p))
            (i -> neighbors)
        }.toMap

        val cells: Map[Int, BSP with ComputeMethod] = genPopulation(adjList)

        partitionPartialGraph(adjList.toIterable.flatMap(i => i._2.map(j => (j, i._1))), adjList.keys.toSet, (0 until totalMachines * baseFactor).map(i => (i, i % baseFactor)).toMap).view.zipWithIndex.map(i => {
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = i._2
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            BSPModel.Optimize.default(part)
        }).map(i => new partActor(i)).toVector
    }
}