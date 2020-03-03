package meta.example.traffic_light_example

import meta.classLifting.SpecialInstructions
import meta.deep.runtime.Actor
import squid.quasi.lift


@lift
class Person() extends Actor{

  var trafficLight: TrafficLight = null

  def wait_to_cross():Unit = {
    if(trafficLight.state == 0){
      println("The person asks to cross the road!")
      trafficLight.toggle()
      println("The person is crossing the road")
      //      SpecialInstructions.waitTurns(1)
    }
    else{
      println("The person is crossing the road")
    }
  }

  def main(): Unit = {
    while(true) {

      //TODO should be a random function


      val rnd = new scala.util.Random
      val r = rnd.nextInt(100)
      if (r % 3 == 1){
        wait_to_cross()
      }
      SpecialInstructions.waitTurns(1)

    }
  }


}
