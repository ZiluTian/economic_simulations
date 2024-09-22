package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{DoubleVectorVectorMessage, Actor}
import BSPModel.example.stockMarket._
import BSPModel.Connector._

class stockMarketFusedTest extends scaleUpTest {

    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]
        // Use List[Double] as the interface message type
        override def run(): Int = {
            receivedMessages.foreach(i => {
                // println(f"$id receives message $i")
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[DoubleVectorVectorMessage].value(0).head.toInt) = i.asInstanceOf[DoubleVectorVectorMessage].value.tail
            })
            receivedMessages.clear()

            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, 
                    DoubleVectorVectorMessage(Vector(part.id.toDouble) +: 
                        i._2.map(j => 
                            fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState match {
                                case x: Double => Vector(x)
                                case x: Vector[Double] => x
                                case _ => 
                                    // println(f"The public state value is ${fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState}")
                                    throw new Exception("Unfound msg type!")
                            })))
            })
            1
        }
    }

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