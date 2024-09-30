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

// Fused aggregator needs to distinguish whether a message is received from 
object stockMarketFusedAggregateScaleOutTest extends scaleOutTest with App {
    import BSPModel.example.stockMarket.ConditionActionRule._

    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }

    class MarketAgent(pos: BSPId, neighbors: Seq[BSPId], initialStockPrice: Double) extends BSP with ComputeMethod {
        type State = Market 
        type InMessage = (Int, Int)
        type SerializeFormat = Vector[Double]

        var state: Market = {
            val stock = new Stock(0.01)
            stock.priceAdjustmentFactor = 0.1 / neighbors.size
            // initial st
            Market(stock, stock.updateMarketInfo(initialStockPrice, stock.getDividend), initialStockPrice, 0, 0, 0)
        }
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 

        // Transform from Vector[Double] to (Int, Int)
        override def deserialize(in: SerializeFormat): InMessage = {
            if (in.size == 1) {
                val m = in.head.toInt
                if (m == buy){
                    (1, 0)
                } else if (m == sell) {
                    (0, 1)
                } else {
                    (0, 0)
                }   
            } else if (in.size ==2) {
                (in(0).toInt, in(1).toInt)
            } else {
                throw new Exception("Shouldn't happen!")
            }
        }

        def partialCompute(ms: Iterable[(Int, Int)]): Option[(Int, Int)] = {
            // println(f"$id partial compute is called with $ms")
            ms match {
                case Nil => None
                case _ => 
                    Some(ms.foldLeft((0, 0)){
                        case ((x, y), (e1, e2)) =>
                            (x+e1, y+e2)              // Add to odd list
                    })
            }
        }

        def updateState(s: Market, m: Option[(Int, Int)]): Market = {
            m match {
                case None => s
                case Some(x) => {
                    s.buyOrders += x._1
                    s.sellOrders += x._2
                }
            }
            s.stockPrice = s.stock.priceAdjustment(state.buyOrders, state.sellOrders)
            // println(f"Stock price is ${s.stockPrice} buy ${state.buyOrders} sell ${state.sellOrders}")
            s.dividendPerShare = s.stock.getDividend()
            s.marketState = s.stock.updateMarketInfo(s.stockPrice, s.dividendPerShare)
            state.buyOrders = 0
            state.sellOrders = 0
            s
        }

        def stateToMessage(s: Market):SerializeFormat = {
            Vector[Double](s.stockPrice, s.dividendPerShare, s.marketState(0), s.marketState(1), s.marketState(2))
        }
    } 

    // aggregates values from traders in the neighbors    
    class ActionAggregator(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with ComputeMethod {
        type State = (Int, Int)
        type InMessage = (Int, Int)
        type SerializeFormat = Vector[Double]

        var state = (0, 0)
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 

        // receive 
        // Transform from Vector[Double] to (Int, Int)
        override def deserialize(in: SerializeFormat): InMessage = {
            val m = in.head.toInt
            if (m == buy){
                (1, 0)
            } else if (m == sell) {
                (0, 1)
            } else {
                (0, 0)
            }
        }

        def partialCompute(ms: Iterable[(Int, Int)]): Option[(Int, Int)] = {
            // println(f"$id partial compute is called with $ms")
            ms match {
                case Nil => None
                case _ => 
                    Some(ms.foldLeft((0, 0)){
                        case ((x, y), (e1, e2)) =>
                            (x+e1, y+e2)              // Add to odd list
                    })
            }
        }

        def updateState(s: State, m: Option[(Int, Int)]): State = {
            m match {
                case None => s
                case Some(x) => {
                    (s._1 + x._1, s._2 + x._2)
                }
            }
        }

        def stateToMessage(s: State):SerializeFormat = {
            Vector[Double](s._1, s._2)
        }
    }

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

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001
        var adjList = Map[Int, List[Int]]()
        val offset = machineId * baseFactor        
        val elementsPerPartition = baseFactor / localScaleFactor
        val tradersPerPartition = elementsPerPartition - 1
        // Connect the market/proxy with local traders
        (0 until localScaleFactor).foreach(i => {
            adjList = adjList + ((offset + i * elementsPerPartition) -> (0 :: ((offset + i * elementsPerPartition + 1) until (offset + (i+1) * elementsPerPartition)).toList))
            ((offset + i * elementsPerPartition + 1) until (offset + (i+1) * elementsPerPartition)).foreach(j => {
                adjList = adjList + (j -> List(0))
            })
        })

        // 0 should be connected with proxy markets (0, List(10, 20, 30))
        if (machineId == 0) {
            adjList = adjList + (0 -> (adjList(0).filter(_!=0) ++ (1 until totalMachines * localScaleFactor).map(i => i * elementsPerPartition)).toList)
        }

        val cells: Map[Int, BSP with ComputeMethod] = 
            adjList.map(i => {
                if (i._1 == 0) {
                    (i._1, new MarketAgent(i._1, i._2, initialStockPrice))
                } else if (i._1 % elementsPerPartition ==0) {
                    (i._1, new ActionAggregator(i._1, i._2))
                } else {
                    (i._1, new TraderAgent(i._1, i._2, budget, interestRate))
                }
            })

        (0 until localScaleFactor).map(i => {
            val globalId = machineId * localScaleFactor + i
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = globalId
                val topo = new BSPModel.Graph[BSPId] {
                    val vertices = (globalId*elementsPerPartition until (globalId+1)*elementsPerPartition).toSet
                    val edges = Map()
                    val inExtVertices = if (globalId == 0) {
                        // Receive aggregated action from other partitions
                        (1 until totalMachines * localScaleFactor).map(j => {
                            (j, Vector(j*elementsPerPartition))
                        }).toMap    
                    } else {
                        // Receive market state from 0
                        Map(0 -> Vector(0))
                    }
                    val outIntVertices = if (globalId == 0) {
                        // Send the market value to other partitions
                        (1 until totalMachines * localScaleFactor).map(j => {
                            (j, Vector(0))
                        }).toMap
                    } else {
                        // Send aggregated action to partition 0
                        Map(0 -> Vector(globalId*elementsPerPartition))
                    }
                }
                // println(f"Partition ${i} has vertices ${topo.vertices} ${topo.inExtVertices}")
                val members = (globalId*elementsPerPartition until (globalId+1)*elementsPerPartition).map(j => cells(j)).toList
            }
            BSPModel.Optimize.default(part)
        // partition(graph, localScaleFactor).view.zipWithIndex.map(i => {
        }).map(i => new partActor(i)).toVector
    }
}