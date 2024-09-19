package example
package stockMarket
package v2

object Example extends App {
    val liftedMain = meta.classLifting.liteLift {
        def apply(totalMarkets: Int, tradersPerMarket: Int): IndexedSeq[Actor] = {
            val initialWealth: Double = 1000
            val interestRate: Double = 0.001

            (0 until totalMarkets).flatMap(i => {
                val traders = (1 to tradersPerMarket).map(x => new Trader(initialWealth, interestRate))
                val market = new Market()
                market.connectedAgentIds = traders.map(i => i.id.toInt).map(_.toLong)
                market +: traders
            })
        }
    }
    
    val cls1: ClassWithObject[Market] = Market.reflect(IR)
    val cls2: ClassWithObject[Trader] = Trader.reflect(IR)

    compileSims(List(cls1, cls2), Some(liftedMain))
}