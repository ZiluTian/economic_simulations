package simulation.akka
package test

import org.scalatest.FlatSpec
import simulation.akka.API._
import meta.API.DeforestationStrategy

class shortestPath extends FlatSpec {
    val totalVertices: Int = 50
    val totalRounds: Int = 50
    
    f"The single source shortest path algorithm over a linked list with ${totalVertices} vertices, sequential workers" should f"update the distance of all vertices in ${totalVertices} rounds" in {
        case class Distance(dist: Int) extends Serializable
        object ShortestPathOptStrategy extends DeforestationStrategy {
            // a sequential worker applies the mapper to each agent
            override def mapper(x: Serializable): Serializable = {
                Distance(x.asInstanceOf[generated.core.test.shortestPath.Vertex].dist)
            }
            // the driver sends an Iterable[Serializable] to the log controller. Log controller collects Iterable[Iterable[Serializable]]
            // and applies the reducer method to reduce the intermediate data 
            override def reducer(x: Iterable[Iterable[Serializable]]): Iterable[Serializable] = {
                x.flatten
            }
        }

        val agents = generated.core.test.shortestPath.InitData()
        val conf = Map("role" -> "Standalone", 
            "port" -> 25300, 
            "name" -> "ShortestPath", 
            "data" -> "timeseries")
        val ts = API.Simulate(agents, totalRounds, conf)(ShortestPathOptStrategy)
        ts.timeseries.foreach(t => { println(t) })
    }
}