package BSPModel
package example
package counter

trait CounterCompute extends ComputeMethod {
    type State = Int
    type InMessage = Int
    type SerializeFormat = Int

    def partialCompute(m1: Iterable[Int]): Option[Int] = {
        if (m1.isEmpty) {
            None
        } else {
            Some(m1.sum)
        }
    }

    def updateState(s: Int, m: Option[Int]): Int = {
        m match {
            case None => s
            case Some(x) =>
                s + x
        }
    }

    def stateToMessage(s: Int): Int = {
        s
    }
}

class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with CounterCompute {
    var state: Int = 1
    override val id = pos
    val receiveFrom = FixedCommunication(neighbors) 
} 
