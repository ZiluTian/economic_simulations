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
import BSPModel.example.stockMarket.ConditionActionRule._
import BSPModel.Connector._

class stockMarketGraphTest extends scaleUpTest {
    val budget: Double = 1000
    val initialStockPrice: Double = 100
    val interestRate: Double = 0.0001

    // Hard-code market to be at partition 0
    // Other markets are placeholders
    class stockMarketPartition(partId: Int, traders: Int) extends Actor {
        // The role of the placeholder in partitions without market is to perform in-network aggregation and message caching
        val numMarket: Int = 1
        id = partId

        val world: Array[Any] =
            ((0 until numMarket).map(i => {
                val stock = new Stock(0.01)
                stock.priceAdjustmentFactor = 0.1 / traders
                Market(stock, stock.updateMarketInfo(initialStockPrice, stock.getDividend), initialStockPrice, 0, 0, 0)
            }) ++ (0 until traders).map(i => {
                Trader(new WealthManagement(budget, interestRate), 0)
            })).toArray
        
        override def run(): Int = {
            if (partId == 0) {
                receivedMessages.foreach(i => {
                    // messages have been locally aggregated
                    world(0).asInstanceOf[Market].buyOrders += i.asInstanceOf[DoubleVectorMessage].value(0).toInt
                    world(0).asInstanceOf[Market].sellOrders += i.asInstanceOf[DoubleVectorMessage].value(1).toInt
                })
                receivedMessages.clear()
                // Run trader and market agents
                (numMarket until traders + numMarket).foreach(traderIdx => {
                    (0 until numMarket).foreach(marketIdx => {
                        world(traderIdx).asInstanceOf[Trader].wealth.addDividends(world(marketIdx).asInstanceOf[Market].dividendPerShare)
                        world(traderIdx).asInstanceOf[Trader].action = world(traderIdx).asInstanceOf[Trader].wealth.takeAction(world(marketIdx).asInstanceOf[Market].stockPrice, world(marketIdx).asInstanceOf[Market].marketState)
                    })
                    world(traderIdx).asInstanceOf[Trader].wealth.addInterest()
                })
                // Each market gets the number of buy and sell orders from traders and computes the new stock price
                (0 until numMarket).foreach(marketIdx => {
                    // get the number of buys and sells from traders
                    (numMarket until traders + numMarket).foreach(traderIdx => {
                        if (world(traderIdx).asInstanceOf[Trader].action == buy) {
                            world(marketIdx).asInstanceOf[Market].buyOrders += 1
                        } else if (world(traderIdx).asInstanceOf[Trader].action == sell) {
                            world(marketIdx).asInstanceOf[Market].sellOrders += 1
                        }
                    }) 
                    // println(f"${world(marketIdx).asInstanceOf[Market].buyOrders} buys ${world(marketIdx).asInstanceOf[Market].sellOrders} sells")
                    world(marketIdx).asInstanceOf[Market].stockPrice = world(marketIdx).asInstanceOf[Market].stock.priceAdjustment(world(marketIdx).asInstanceOf[Market].buyOrders, world(marketIdx).asInstanceOf[Market].sellOrders)
                    // println(f"Stock price is ${world(marketIdx).asInstanceOf[Market].stockPrice}")
                    world(marketIdx).asInstanceOf[Market].dividendPerShare = world(marketIdx).asInstanceOf[Market].stock.getDividend()
                    world(marketIdx).asInstanceOf[Market].marketState = world(marketIdx).asInstanceOf[Market].stock.updateMarketInfo(world(marketIdx).asInstanceOf[Market].stockPrice, world(marketIdx).asInstanceOf[Market].dividendPerShare)
                    // clear previous buy and sell orders
                    world(marketIdx).asInstanceOf[Market].buyOrders = 0
                    world(marketIdx).asInstanceOf[Market].sellOrders = 0

                    val message = DoubleVectorMessage(Vector(
                        world(marketIdx).asInstanceOf[Market].stockPrice, 
                        world(marketIdx).asInstanceOf[Market].dividendPerShare,
                        world(marketIdx).asInstanceOf[Market].marketState(0), 
                        world(marketIdx).asInstanceOf[Market].marketState(1),
                        world(marketIdx).asInstanceOf[Market].marketState(2)
                    ))

                    connectedAgentIds.foreach(a => {
                        sendMessage(a, message)
                    })
                })                
            } else {
                // clear buffer
                world(0).asInstanceOf[Market].buyOrders = 0
                world(0).asInstanceOf[Market].sellOrders = 0
                // use received messages to update the state of the placeholder
                receivedMessages.foreach(i => {
                    val marketMessage: Vector[Double] = i.asInstanceOf[DoubleVectorMessage].value
                    // put the message in the market placeholder
                    world(0).asInstanceOf[Market].stockPrice = marketMessage(0)
                    world(0).asInstanceOf[Market].dividendPerShare = marketMessage(1)
                    world(0).asInstanceOf[Market].marketState = List(marketMessage(2).toInt, marketMessage(3).toInt, marketMessage(4).toInt)
                })
                receivedMessages.clear()
                (1 until traders + numMarket).foreach(traderIdx => {
                    // Partially aggregate the result in market placeholders 
                    (0 until numMarket).foreach(marketIdx => {
                        world(traderIdx).asInstanceOf[Trader].wealth.addDividends(world(marketIdx).asInstanceOf[Market].dividendPerShare)
                        world(traderIdx).asInstanceOf[Trader].action = world(traderIdx).asInstanceOf[Trader].wealth.takeAction(world(marketIdx).asInstanceOf[Market].stockPrice, world(marketIdx).asInstanceOf[Market].marketState)
                        if (world(traderIdx).asInstanceOf[Trader].action == buy) {
                            world(0).asInstanceOf[Market].buyOrders += 1
                        } else if (world(traderIdx).asInstanceOf[Trader].action == sell) {
                            world(0).asInstanceOf[Market].sellOrders += 1
                        }
                    })
                    world(traderIdx).asInstanceOf[Trader].wealth.addInterest()
                })

                val message = new DoubleVectorMessage(Vector(world(0).asInstanceOf[Market].buyOrders, world(0).asInstanceOf[Market].sellOrders))
                sendMessage(0, message)               
            }
            1
        }
    }

    def gen(scaleUpFactor: Int): IndexedSeq[Actor] = {
        val traders: Int = baseFactor * scaleUpFactor - 1

        val baseTraders: Int = traders / scaleUpFactor
        val remainingTraders: Int = traders % scaleUpFactor

        (0 until scaleUpFactor).map { id =>
            val traders = if (id < remainingTraders) baseTraders + 1 else baseTraders
            val agent = new stockMarketPartition(id, traders)
            // only 0 has the real market agent
            if (id == 0){
                agent.connectedAgentIds = (1 until scaleUpFactor).map(_.toLong)
            }
            agent
        }
    }
}