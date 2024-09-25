package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{IntVectorMessage, IntArrayMessage, Actor}
import BSPModel.example.gameOfLife._
import BSPModel.Connector._

object gameOfLifeFusedScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }

    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]

        override def run(): Int = {
            receivedMessages.foreach(i => {
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[IntVectorMessage].value(0)) = i.asInstanceOf[IntVectorMessage].value.tail
            })
            receivedMessages.clear()
            
            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, IntVectorMessage(part.id +: i._2.map(j => fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState.asInstanceOf[Int])))
            })
            1
        }
    }

    // Per local machine, partition into 50 components
    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val localScaleFactor: Int = 10
        val height: Int = ((baseFactor / width) /localScaleFactor).toInt
        val totalHeight: Int = (baseFactor * totalMachines / width).toInt
        // println("Vertices are " + graph.nodes)
        // println("Incoming external vertices are " + graph.adjacencyList)
        // println("Graph edges are " + graph.edges)

        val startingIndex = machineId * baseFactor
        // val graph = GraphFactory.torus2D(width, totalHeight)
        // val cells: Map[Int, BSP with ComputeMethod] = toGraphInt(graph.adjacencyList()).map(i => (i._1, new Cell(i._1, i._2.toSeq))).toMap
        // partition(graph, totalMachines * localScaleFactor).view.zipWithIndex.map(i => {
        //     val part = new BSPModel.Partition {
        //         type Member = BSP with ComputeMethod
        //         type NodeId = BSPId
        //         type Value = BSP
        //         val id = i._2
        //         val topo = i._1
        //         val members = i._1.vertices.map(j => cells(j)).toList
        //     }
        //     BSPModel.Optimize.default(part)
        // }).toVector.slice(startingIndex, localScaleFactor + startingIndex).map(i => new partActor(i))

        val adjList = (startingIndex until width * height + startingIndex).map(index => {
            val x: Int = index % width
            val y: Int = index / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + height) % totalHeight
            } yield dy * width + dx
            (index + startingIndex, neighbors.map(n => n + startingIndex).toVector)
        }).toMap

        val cells: Map[Int, BSP with ComputeMethod] = adjList.map(i => (i._1, new Cell(i._1, i._2)))

        // val graph = new cloudcity.lib.Graph.Graph {
        //     def adjacencyList() = adjList.map(i => (i._1.toLong, i._2.map(_.toLong)))
        //     def nodes = adjList.keys.map(_.toLong)
        //     def edges = adjList.toIterable.flatMap { case (node, neighbors) =>
        //         neighbors.map(neighbor => (node.toLong, neighbor.toLong))
        //     }
        // }

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