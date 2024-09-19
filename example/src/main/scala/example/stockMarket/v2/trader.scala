package example
package stockMarket
package v2

import squid.quasi.lift
import meta.runtime.{DoubleVectorMessage, IntMessage}
import meta.classLifting.SpecialInstructions._

@lift 
class Trader(var budget: Double, val interestRate: Double) extends Actor {

    var wealth: WealthManagement = null
    
    def main(): Unit = {
        wealth = new WealthManagement(budget, interestRate)
        while (true) {
            var m = receiveMessage()
            while (m.isDefined){
                var ans = m.get.asInstanceOf[DoubleVectorMessage].value
                wealth.addDividends(ans(2))
                // For each received message, sends a reply. Hence only need to increase the cfreq. of market for microbenchmark
                val action = wealth.takeAction(ans(1), List(ans(3).toInt, ans(4).toInt, ans(5).toInt))
                sendMessage(ans(0).toLong, IntMessage(action))
                m = receiveMessage()
            }

            waitRounds(1)
            wealth.addInterest()
        }
    }
}