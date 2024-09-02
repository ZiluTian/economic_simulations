package simulation.akka
package test

import org.scalatest.FlatSpec
import simulation.akka.API._
import meta.API.{SimulationData, Timeseries}

class shortestPath extends FlatSpec {
    val totalVertices: Int = 50
    val totalRounds: Int = 50
    
    case class Distance(dist: Int) extends Serializable
    case object ShortestPathTimeseries extends SimulationTimeseries {
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

    f"The single source shortest path algorithm over a linked list with ${totalVertices} vertices, sequential workers" should f"update the distance of all vertices in ${totalVertices} rounds" in {
        val agents = generated.core.test.shortestPath.InitData()
        API.OptimizationConfig.timeseriesSchema = ShortestPathTimeseries
        val conf = Map("role" -> "Standalone", 
            "port" -> 25300, 
            "name" -> "ShortestPath", 
            "data" -> "timeseries")
        val ts = API.Simulate(agents, totalRounds, conf)
        ts.timeseries.foreach(t => { println(t) })
    }
}