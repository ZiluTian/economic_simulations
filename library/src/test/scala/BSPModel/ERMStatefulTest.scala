package BSPModel
package test

import BSPModel.example.epidemics._
import BSPModel.Connector._
import cloudcity.lib.Graph.GraphFactory
import scala.util.Random

class ERMStatefulTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    override val totalRounds: Int = 3
    override val experimentName: String = "ERM (opt)"

    test(f"${experimentName} example should run") {
        List(1000).foreach(population => {
            // writer.write(f"Config: population ${population} rounds ${totalRounds} ")

            val graph = GraphFactory.erdosRenyi(population, connectivity)
            val agents = toGraphInt(graph.adjacencyList()).map(i => new PersonAgent(i._1, if (Random.nextInt(100)==0) 0 else 2, i._2.toSeq))

            // binding information (partition structure)
            val initPartition = new Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = 1

                val topo = new BSPModel.Graph[BSPId]{
                    val vertices = graph.nodes
                    val edges = graph.adjacencyList()
                    val inExtVertices = Map()
                    val outIntVertices = Map()
                }

                val members = agents.toList
            }

            val ans = BSPModel.Optimize.default(initPartition)

            benchmarkTool[Unit]( 
                Range(1, totalRounds).foreach(j => {
                    val summary = ans.members.map(_.state.asInstanceOf[Array[BSP with ComputeMethod with DoubleBuffer]]).flatMap(k => k.map(i => i.state.asInstanceOf[Person])).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    println(f"Summary: ${summary}")
                    ans.members.map(i => {
                        i.run(List())
                    })
                    println(f"Round $j")
                    // val summary = ans.members.map(_.state.asInstanceOf[Person]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                })
            ) 
        })
    }
}