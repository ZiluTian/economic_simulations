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
class stockMarketUnoptAggregateTest extends scaleUpTest {
    import BSPModel.example.stockMarket.ConditionActionRule._

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

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001
        
        var adjList = Map[Int, List[Int]]()
        // Original market receives from local traders and proxies
        adjList = adjList + (0 -> ((1 until baseFactor) ++ (1 until scaleUpFactor).map(k => k * baseFactor)).toList)
        
        (1 until baseFactor).foreach(j => {
            adjList = adjList + (j -> List(0))
        })

        (1 until scaleUpFactor).foreach(i => {
            val j: Int = i * baseFactor
            // Add market proxy
            adjList = adjList + (j -> ((j+1) until (i+1)*baseFactor).toList)
            // Traders still receive stock information from the original market
            ((j+1) until (i+1)*baseFactor).foreach(k => {
                adjList = adjList + (k -> List(0))
            })
        })

        adjList.map(i => {
            val agent = if (i._1 == 0) {
                new MarketAgent(i._1, i._2, initialStockPrice)
            } else if (i._1 % baseFactor ==0) {
                new ActionAggregator(i._1, i._2)
            } else {
                new TraderAgent(i._1, i._2, budget, interestRate)
            }
            bspToAgent(agent)
        }).toVector
    }
}