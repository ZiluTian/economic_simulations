package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{DoubleVectorMessage, Actor}
import BSPModel.example.stockMarket._
import BSPModel.Connector._

class stockMarketFusedTest extends scaleUpTest {

    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]

        // Use List[Double] as the interface message type
        override def run(): Int = {
            receivedMessages.foreach(i => {
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[DoubleVectorMessage].value(0).toInt) = i.asInstanceOf[DoubleVectorMessage].value.tail
            })

            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, 
                    DoubleVectorMessage(part.id.toDouble +: 
                        i._2.flatMap(j => 
                            fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState match {
                                case x: Int => Vector(x.toDouble)
                                case x: Vector[Double] => x
                                case _ => throw new Exception("Unfound msg type!")
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