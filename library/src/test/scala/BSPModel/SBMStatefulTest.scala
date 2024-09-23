package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._
import BSPModel.Connector._
import BSPModel.example.epidemics._

class SBMStatefulTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "SBM (opt)"

    test(f"${experimentName} example should run") {
        List(1000).foreach(population => {
            writer.write(f"Config: population ${population} rounds ${totalRounds}\n")
            val graph = toGraphInt(GraphFactory.stochasticBlock(population, connectivity, 0, 5).adjacencyList())
            val agents = (0 until population).map(i => {
                val age: Int = Random.nextInt(90)+10
                new PersonAgent(i, 
                    age, 
                    graph.getOrElse(i, List()).toList, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0)
            })

            // binding information (partition structure)
            val initPartition = new Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = 1

                val topo = new BSPModel.Graph[BSPId]{
                    val vertices = graph.keySet
                    val edges = graph.mapValues(_.toVector)
                    val inExtVertices = Map()
                    val outIntVertices = Map()
                }

                val members = agents.toList
            }

            val ans = BSPModel.Optimize.default(initPartition)

            benchmarkTool[Unit]( 
                writer, 
                Range(1, totalRounds).foreach(_ => {
                    ans.members.map(i => {
                        i.run(List())
                    })
                    // val summary = ans.members.map(_.state.asInstanceOf[Person]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    // val summary = ans.members.map(_.state.asInstanceOf[(Array[BSP with ComputeMethod with DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = BSPId}])]._1).flatMap(k => k.map(i => i.state.asInstanceOf[Person])).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    // println(f"Summary: ${summary}")
                })
            ) 
        })
    }
}