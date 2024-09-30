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
        val totalHeight: Int = (baseFactor * totalMachines / width).toInt
        val height: Int = (baseFactor / width / localScaleFactor).toInt
        val startingIndex = machineId * baseFactor
        // println("Vertices are " + graph.nodes)
        // println("Incoming external vertices are " + graph.adjacencyList)
        // println("Graph edges are " + graph.edges)

        val cells = (startingIndex until startingIndex + baseFactor).map(index => {
            val x: Int = index % width
            val y: Int = index / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + totalHeight) % totalHeight
            } yield dy * width + dx
            (index, new Cell(index, neighbors.toSeq))
        }).toMap

        (0 until localScaleFactor).map(i => {
            val bspGraph = partition2DArray(machineId * localScaleFactor + i, localScaleFactor * totalMachines, width, height)
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = i + machineId * localScaleFactor
                val topo = bspGraph
                val members = bspGraph.vertices.map(j => cells(j)).toList
            }
            val ans = new partActor(BSPModel.Optimize.default(part))
            // println(f"Successfully generated and optimized partition ${i + machineId * localScaleFactor}")
            ans
        }).toVector
    }
}