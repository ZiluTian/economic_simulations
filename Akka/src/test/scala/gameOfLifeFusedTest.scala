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

class gameOfLifeFusedTest extends scaleUpTest {
    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]

        override def run(): Int = {
            receivedMessages.foreach(i => {
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[IntVectorMessage].value(0)) = i.asInstanceOf[IntVectorMessage].value.tail
            })

            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, IntVectorMessage(part.id +: i._2.map(j => fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState.asInstanceOf[Int])))
            })
            1
        }
    }

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val height: Int = scaleUpFactor*(baseFactor / width).toInt

        val graph = GraphFactory.torus2D(width, height)
        // println("Vertices are " + graph.nodes)
        // println("Incoming external vertices are " + graph.adjacencyList)
        // println("Graph edges are " + graph.edges)

        val cells: Map[Int, BSP with ComputeMethod] = toGraphInt(graph.adjacencyList()).map(i => (i._1, new Cell(i._1, i._2.toSeq))).toMap
        partition(graph, scaleUpFactor).view.zipWithIndex.map(i => {
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