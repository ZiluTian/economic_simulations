package BSPModel
package test

import scala.util.Random
import BSPModel.Connector._
import cloudcity.lib.Graph.GraphFactory
import BSPModel.example.epidemics._

class ERMStatefulTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    override val totalRounds: Int = 50
    override val experimentName: String = "ERM (opt)"

    test(f"${experimentName} example should run") {
        List(1000).foreach(population => {
            // writer.write(f"Config: population ${population} rounds ${totalRounds} ")
            val graph = toGraphInt(GraphFactory.erdosRenyi(population, connectivity).adjacencyList())
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
                Range(0, totalRounds).foreach(j => {
                    val summary = ans.members.map(_.state.asInstanceOf[Array[BSP with ComputeMethod with DoubleBuffer]]).flatMap(k => k.map(i => i.state.asInstanceOf[Person])).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    println(f"Summary: ${summary}")
                    ans.members.map(i => {
                        i.run(List())
                    })
                    // ans.members.map(i => {
                    //     i.state.asInstanceOf[Array[BSP with ComputeMethod with DoubleBuffer]].foreach(_.updatePublicState())
                    // })
                    println(f"Round $j")

                    // val summary = ans.members.map(_.state.asInstanceOf[Person]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                })
            ) 
        })
    }
}