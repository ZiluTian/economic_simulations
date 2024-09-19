package example
package stockMarket
package v2

import squid.quasi.lift
import meta.classLifting.SpecialInstructions._
import meta.runtime.DoubleVectorMessage

@lift 
class Market() extends Actor {

    val stock: Stock = new Stock(0.01)
    private var marketState: List[Int] = List()
    // Initial price
    var stockPrice: Double = 100
    var dividendPerShare: Double = 0
    var buyOrders: Int = 0
    var sellOrders: Int = 0

    def main(): Unit = {
        stock.priceAdjustmentFactor = 0.1 / connectedAgentIds.size
        while (true) {
            buyOrders = 0
            sellOrders = 0
            var m = receiveMessage()
            while (m.isDefined){
                var ans = m.get.value
                if (ans == 1) {
                    buyOrders = buyOrders + 1
                } 
                if (ans == 2) {
                    sellOrders = sellOrders + 1
                }
                m = receiveMessage()
            }
            stockPrice = stock.priceAdjustment(buyOrders, sellOrders)
            dividendPerShare = stock.getDividend()
            marketState = stock.updateMarketInfo(stockPrice, dividendPerShare)
            
            val msg = Vector[Double](
                id.toDouble,
                stockPrice,
                dividendPerShare,
                marketState(0).toDouble,
                marketState(1).toDouble,
                marketState(2).toDouble)

            connectedAgentIds.foreach(i => {
                sendMessage(i, DoubleVectorMessage(msg))
            })
            waitRounds(1)
        }
    }
}

