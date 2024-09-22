package BSPModel
package example
package stockMarket

import ConditionActionRule._

case class Market(
    val stock: Stock,
    var marketState: List[Int],
    var stockPrice: Double,
    var dividendPerShare: Double,
    var buyOrders: Int,
    var sellOrders: Int,
)

case class Trader(
    val wealth: WealthManagement, 
    var action: Int)

// Out messages of agents should be the SerializeFormat messages of others
// SerializeFormat denotes the serialized format, which is the same for both i/o
class TraderAgent(pos: BSPId, neighbors: Seq[BSPId], budget: Double, interestRate: Double) extends BSP with ComputeMethod {
    type State = Trader   
    type InMessage = Vector[Double]
    type SerializeFormat = Vector[Double]

    var state: Trader = Trader(new WealthManagement(budget, interestRate), 0)
    override val id = pos
    val receiveFrom = FixedCommunication(neighbors) 

    def partialCompute(ms: Iterable[Vector[Double]]): Option[Vector[Double]] = {
        // println(f"partial compute in $pos is invoked with $ms")
        ms match {
            case Nil => None 
            case _ => Some(ms.head)
        }
    }

    def updateState(s: Trader, m: Option[Vector[Double]]): Trader = {
        // println(f"Update state is invoked in trader $id with message $m")
        m match {
            case None => s
            case Some(x) => {
                // println(f"Received state is $x")
                s.wealth.addDividends(x(1))
                s.action = s.wealth.takeAction(x(0), List(x(2).toInt, x(3).toInt, x(4).toInt))
            }
        }
        s.wealth.addInterest()
        // println(f"Trader action is ${s.action}")
        s
    }

    def stateToMessage(s: Trader): SerializeFormat = {
        Vector(s.action.toDouble)
    }
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

    override def deserialize(in: SerializeFormat): InMessage = {
        val m = in.head.toInt
        if (m == buy){
            (m, 0)
        } else if (m == sell) {
            (0, m)
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