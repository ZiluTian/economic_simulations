package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{DoubleVectorMessage, IntArrayMessage, Actor}
import BSPModel.example.epidemics._
import BSPModel.Connector._

abstract class EpidemicsFusedScaleOutTest extends scaleOutTest {

    class partActor(part: BSPModel.Partition) extends Actor {
        id = part.id.toLong
        val fusedGraphAgent = part.members.head.asInstanceOf[BSP with ComputeMethod]

        override def run(): Int = {
            receivedMessages.foreach(i => {
                part.topo.asInstanceOf[ArrayGraph[BSPId]].inCache(i.asInstanceOf[DoubleVectorMessage].value(0).toInt) = i.asInstanceOf[DoubleVectorMessage].value.tail
            })
            receivedMessages.clear()
            
            fusedGraphAgent.run(List())

            part.topo.outIntVertices.foreach(i => {
                sendMessage(i._1, DoubleVectorMessage(part.id.toDouble +: i._2.map(j => fusedGraphAgent.state.asInstanceOf[Array[BSP with ComputeMethod with Stage with DoubleBuffer]](j.asInstanceOf[Int]).asInstanceOf[BSP with ComputeMethod with Stage with DoubleBuffer].publicState.asInstanceOf[Double])))
            })
            1
        }
    }

    def genPopulationHashed(g: Map[Int, Iterable[Int]], partitionSize: Int): Map[Int, List[BSP with ComputeMethod]] = {
        var ans = Map[Int, List[BSP with ComputeMethod]]()
        g.foreach(i => {
            val age: Int = Random.nextInt(90)+10
            val v = new PersonAgent(i._1, 
                    age, 
                    i._2.toVector, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0)
            ans = ans + ((i._1 / partitionSize) -> (v :: ans.getOrElse(i._1 / partitionSize, List[BSP with ComputeMethod]())))
        })
        ans
    }

    def genPopulation(g: Map[Int, Iterable[Int]]): Map[Int, BSP with ComputeMethod] = {
        g.map(i => {
            val age: Int = Random.nextInt(90)+10
            (i._1, new PersonAgent(i._1, 
                    age, 
                    i._2.toVector, 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    if (age > 60) 1 else 0))
        })
    }
}

object ERMFusedScaleOutTest extends EpidemicsFusedScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    lazy val cuts: (Int, Int) => Map[BSPId, Set[BSPId]] = (partitionSize: Int, totalPartitions: Int) => {
        val p: Double = 0.01
        val rand = new Random(100)
        var graph = Map.empty[BSPId, Set[BSPId]]
        (0 until totalPartitions).foreach ({ i => 
            (0 until partitionSize).foreach(v => {
                val vid = i * partitionSize + v
                val neighbors = ((i + 1) * partitionSize until totalPartitions * partitionSize).filter(_ => rand.nextDouble() < p)
                graph = graph + ((i * partitionSize + v) -> (graph.getOrElse(vid, Set()) ++ neighbors))
                neighbors.foreach(n => {
                    graph = graph + (n -> (graph.getOrElse(n, Set()) + vid))
                })
            })
        })
        graph
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val startingIndex = machineId * baseFactor
        val partitionSize = baseFactor / localScaleFactor
        val crossPartitionEdges = cuts(baseFactor / localScaleFactor, localScaleFactor * totalMachines)
        println(f"Cross partition edges on $machineId have been computed!")
        var graph: Map[BSPId, Iterable[BSPId]] =
            toGraphInt(GraphFactory.erdosRenyi(baseFactor, p, startingIndex).adjacencyList)
            .map(i => (i._1, crossPartitionEdges.getOrElse(i._1, Set()) ++ i._2))
        println(f"Graph at $machineId has been constructed!")

        // Generate a partial graph
        // val adjList = (startingIndex until (startingIndex + baseFactor)).view.map { i =>
        //     val neighbors1 = ((0 until startingIndex) ++ (startingIndex + baseFactor until baseFactor * totalMachines)).filter(_ => rand.nextDouble() < p)
        //     val neighbors2 = (startingIndex until (startingIndex + baseFactor)).filter(j => i != j && rand.nextDouble() < p)
        //     (i -> (neighbors1, neighbors2))
        // }.toMap
        val cells: Map[Int, List[BSP with ComputeMethod]] = genPopulationHashed(graph, partitionSize)
        val edges: Iterable[(BSPId, BSPId)] = graph.toIterable.flatMap { case (node, neighbors) =>
            neighbors.flatMap(neighbor => if (neighbor / partitionSize != node / partitionSize) List((node, neighbor), (neighbor, node)) else List())
        }.toSet
        // val crossEdges = adjList.toIterable.flatMap(i => i._2._1.flatMap(j => List((i._1, j), (j, i._1)))).toSet

        val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)

        partitionPartialGraph(edges, partIds, partitionSize).view.zipWithIndex.map(i => {
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = partIds(i._2)
                val topo = i._1
                val members = cells(i._2)
            }
            println(f"Local partition ${partIds(i._2)} at $machineId has been constructed!")
            new partActor(BSPModel.Optimize.default(part))
        }).toVector
    }
}

object SBMFusedScaleOutTest extends EpidemicsFusedScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val q: Double = 0
        val startingIndex = machineId * baseFactor
        val partitionSize = baseFactor / localScaleFactor
        val graph = GraphFactory.stochasticBlock(baseFactor, p, q, 5, startingIndex)
        val edges: Iterable[(BSPId, BSPId)] = graph.edges.filter { case (node, neighbor) =>
            neighbor / partitionSize != node / partitionSize
        }.toSet

        // val graph = GraphFactory.erdosRenyi(baseFactor, p, startingIndex)
        // val cells: Map[Int, BSP with ComputeMethod] = genPopulation(toGraphInt(graph.adjacencyList))
        val cells: Map[Int, List[BSP with ComputeMethod]] = genPopulationHashed(toGraphInt(graph.adjacencyList), partitionSize)
        val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)

        partitionPartialGraph(edges, partIds, partitionSize).view.zipWithIndex.map(i => {
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = partIds(i._2)
                val topo = i._1
                val members = cells(i._2)
            }
            println(f"Local partition ${partIds(i._2)} at $machineId has been constructed!")
            new partActor(BSPModel.Optimize.default(part))
        }).toVector

        // partition(graph, localScaleFactor, machineId * localScaleFactor).view.zipWithIndex.map(i => {
        //     val partId = i._2 + machineId * localScaleFactor
        //     // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
        //     // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
        //     val part = new BSPModel.Partition {
        //         type Member = BSP with ComputeMethod
        //         type NodeId = BSPId
        //         type Value = BSP
        //         val id = partId
        //         val topo = i._1
        //         val members = i._1.vertices.map(j => (j, cells(j))).toMap
        //     }
        //     new partActor(BSPModel.Optimize.default(part))
        // }).toVector
    }
}