package BSPModel
package example
package gameOfLife

import scala.util.Random

trait GoLCompute extends ComputeMethod {
    type State = Boolean
    type InMessage = Int
    type OutMessage = Int
    
    def partialCompute(m1: Iterable[Int]): Option[Int] = {
        // println(f"Messages received are ${m1}")
        m1 match {
            case Nil => None
            case _ => Some(m1.fold(0)(_+_))
        }
    }

    def updateState(s: Boolean, m: Option[Int]): Boolean = {
        m match {
            case None => {
                s
            }
            case Some(totalAlive) =>     
                if (totalAlive == 3) {
                    true
                } else if (totalAlive < 3 || totalAlive > 3) {
                    false
                } else {
                    s
                }
        }
    }

    def stateToMessage(s: Boolean): Int = {
        if (s) 1 else 0
    }
}

class Cell(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with GoLCompute {
    var state: Boolean = Random.nextBoolean()
    override val id = pos
    val receiveFrom = FixedCommunication(neighbors) 
}