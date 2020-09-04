package meta.example.segregation

import meta.classLifting.SpecialInstructions._
import meta.deep.runtime.{Actor}
import squid.quasi.lift
import segregationModel.Loc

@lift
class Person(var world: WorldMap, var view: Int) extends Actor {

  var location: Loc = -1
  var similarity: Double = 1

  def register(): Unit = {
    location = world.registerLocation(this)
  }

  // happy: same views 50%+
  // notify the world about its similarity
  def unhappy(): Boolean = {
    var sameView: Int = 0
    val neighbors: List[Person] = world.getNeighbors(location)

    neighbors.foreach(n => {
      if (n.view == view){
        sameView = sameView + 1
      }})
    // If only one person, similarity = 1, happy
    if (neighbors.length == 0){
      similarity = 1
      asyncMessage(() => world.report(location, similarity))
      false
    } else {
      similarity = sameView.toDouble/neighbors.length
      asyncMessage(() => world.report(location, similarity))
      sameView < segregationModel.toleranceThreshold * neighbors.length
    }
  }

  def main(): Unit ={
    while(true){
      if (location == -1){
        register()
      } else {
        if (unhappy()){
//          println(id + " I want to move! " + location)
          location = world.move(location)
        }
        waitLabel("People", 1)
//        waitTime(1)
      }
//      waitTurns(1)
      handleMessages()
    }
  }
}