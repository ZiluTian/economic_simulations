package meta.example.supermarket.goods

import meta.classLifting.SpecialInstructions
import squid.quasi.lift

@lift
class Item5 extends Item with Cucumber {
  var age: Int = 0

  def main(): Unit = {
    while(age < freshUntil && !state.isConsumed) {
        itemInfo
        SpecialInstructions.waitTurns(1)
        age = age + 1
    }
    cleanExpired
  }
}