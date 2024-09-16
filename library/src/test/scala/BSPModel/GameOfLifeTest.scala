package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._

class GoLTest extends BSPBenchSuite {
    val totalRounds: Int = 200
    val experimentName: String = "Game of life (opt)"

    trait GoLCompute extends StatelessComputeMethod {
        type State = Boolean
        type Message = Int

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

    test("Game of life example should change the state of population in every round") {
        List((100, 100)).foreach(i => {
            i match {
                case (width, height) => {
                    val partitions: Int = 50

                    writer.write(f"Config: width ${width} height ${height} partitions ${partitions} rounds ${totalRounds} ")
                    val graph = GraphFactory.torus2D(width, height)
                    val agents = toGraphInt(graph.adjacencyList()).map(i => new Cell(i._1, i._2.toSeq))

                    val initPartition = new Partition {
                        type Member = BSP with ComputeMethod
                        type NodeId = BSPId
                        type Value = BSP
                        val id = 1

                        val topo = new BSPModel.Graph[BSPId]{
                            val vertices = graph.nodes
                            val edges = toVectorGraphInt(graph.adjacencyList())
                            val inExtVertices = Map()
                            val outIntVertices = Map()
                        }

                        val members = agents.toList
                    }

                    val ans = BSPModel.Optimize.default(initPartition)

                    benchmarkTool[Unit]( 
                        Range(1, totalRounds).foreach(_ => {
                            ans.members.map(i => {
                                i.run(List())
                            })
                        })
                    ) 
                }
        }})
    }
}