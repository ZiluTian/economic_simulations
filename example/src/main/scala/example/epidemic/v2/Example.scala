package example
package epidemic
package v2

import scala.util.Random
import cloudcity.lib.Graph.LoadGraph

object MainInit {
    val liftedMain = meta.classLifting.liteLift {
        def apply(socialGraph: cloudcity.lib.Graph.Graph): IndexedSeq[Actor] = {
            val citizens = socialGraph.adjacencyList().map(i => {
                val person = new Person(Random.nextInt(90) + 10)
                person.id = i._1
                person
            })
            citizens.map(c => {
                c.connectedAgentIds = socialGraph.adjacencyList()(c.id)
            })
            citizens.toVector
            // var edges = LoadGraph(edgeFilePath)
            // edges.map(i => {
            //     val person = new Person(Random.nextInt(90) + 10)
            //     person.id = i._1
            //     person.connectedAgentIds = i._2
            //     person
            // }).toVector
        }
    }
}

object Example extends App {
  val cls1: ClassWithObject[Person] = Person.reflect(IR)
  val mainClass = MainInit.liftedMain
  compileSims(List(cls1), Some(mainClass))
}