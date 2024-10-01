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
        val p: Double = 0.005
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

    lazy val ermGraph = GraphFactory.erdosRenyi(baseFactor * 2, 0.005)
    lazy val partitionedERMGraph = partition(ermGraph, 2 * localScaleFactor)

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.005
        println(f"Cells at $machineId has been constructed!")
        val partitionedGraphs = partitionedERMGraph.slice(machineId * localScaleFactor, (machineId + 1) * localScaleFactor).par
        // val cells = genPopulation(partitionedGraphs.flatMap(i => i.vertices).map(i => (i, ermGraph.adjacencyList().getOrElse(i, List()))).seq)

        partitionedGraphs.zipWithIndex.map(i => {
            val partId = localScaleFactor * machineId + i._2
            // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
            // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = partId
                val topo = i._1
                val members = genPopulation(i._1.vertices.map(j => (j, ermGraph.adjacencyList().getOrElse(j, List()).map(_.toInt))).toMap).values.toList
            }
            println(f"Local partition ${partId} at $machineId has been constructed!")
            new partActor(BSPModel.Optimize.default(part))
        }).seq.toVector

        // val startingIndex = machineId * baseFactor
        // val partitionSize = baseFactor / localScaleFactor
        // val crossPartitionEdges = cuts(partitionSize, localScaleFactor * totalMachines)
        // println(f"Cross partition edges on $machineId have been computed!")

        // Greedy min k-cut 
        // val cells: Map[Int, BSP with ComputeMethod] = genPopulation(graph)
        // partition(graph, localScaleFactor, localScaleFactor*machineId).zipWithIndex.par.map(i => {
        //     val partId = localScaleFactor * machineId + i._2
        //     // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
        //     // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
        //     val part = new BSPModel.Partition {
        //         type Member = BSP with ComputeMethod
        //         type NodeId = BSPId
        //         type Value = BSP
        //         val id = partIds(i._2)
        //         val topo = i._1
        //         val members = i._1.vertices.map(j => cells(j)).toList
        //     }

        //     new partActor(BSPModel.Optimize.default(part))
        // }).seq.toVector

        // val graph: Map[BSPId, Iterable[BSPId]] =
        //     toGraphInt(GraphFactory.erdosRenyi(baseFactor, p, startingIndex).adjacencyList)
        //     .map(i => (i._1, crossPartitionEdges.getOrElse(i._1, Set()) ++ i._2))
        // println(f"Graph at $machineId has been constructed!")
        // // Hash-based partition
        // val cells: Map[Int, List[BSP with ComputeMethod]] = genPopulationHashed(graph, partitionSize)
        // val edges: Iterable[(BSPId, BSPId)] = graph.toIterable.flatMap { case (node, neighbors) =>
        //     neighbors.flatMap(neighbor => if ((neighbor / partitionSize) != (node / partitionSize)) List((node, neighbor), (neighbor, node)) else List())
        // }.toSet
        // val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)

        // partitionPartialGraph(edges, partIds, partitionSize).zipWithIndex.par.map(i => {
        //     // println(cells.map(i => (i._1, i._2.map(_.id))))
        //     // println(f"Partitions are ${i._1.vertices}")
        //     // assert(cells.getOrElse(machineId * localScaleFactor + i._2, List()).size == i._1.vertices.size)
        //     val part = new BSPModel.Partition {
        //         type Member = BSP with ComputeMethod
        //         type NodeId = BSPId
        //         type Value = BSP
        //         val id = partIds(i._2)
        //         val topo = i._1
        //         val members = cells.getOrElse(machineId * localScaleFactor + i._2, List())
        //     }
        //     println(f"Local partition ${partIds(i._2)} at $machineId has been constructed!")
        //     new partActor(BSPModel.Optimize.default(part))
        // }).seq.toVector
    }
}
object SBMFusedScaleOutTest extends EpidemicsFusedScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.005
        val q: Double = 0
        val startingIndex = machineId * baseFactor
        val partitionSize = baseFactor / localScaleFactor
        val graph = GraphFactory.stochasticBlock(baseFactor, p, q, 5, startingIndex)

        // val cells: Map[Int, List[BSP with ComputeMethod]] = genPopulationHashed(toGraphInt(graph.adjacencyList), partitionSize)
        // val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)
        // val edges: Iterable[(BSPId, BSPId)] = graph.edges.filter { case (node, neighbor) =>
        //     (neighbor / partitionSize) != (node / partitionSize)
        // }.toSet
        // partitionPartialGraph(edges, partIds, partitionSize).zipWithIndex.par.map(i => {
        //     val part = new BSPModel.Partition {
        //         type Member = BSP with ComputeMethod
        //         type NodeId = BSPId
        //         type Value = BSP
        //         val id = partIds(i._2)
        //         val topo = i._1
        //         val members = cells(machineId * localScaleFactor + i._2)
        //     }
        //     println(f"Local partition ${partIds(i._2)} at $machineId has been constructed!")
        //     new partActor(BSPModel.Optimize.default(part))
        // }).seq.toVector

        val cells: Map[Int, BSP with ComputeMethod] = genPopulation(toGraphInt(graph.adjacencyList))
        partition(graph, localScaleFactor, machineId * localScaleFactor).zipWithIndex.par.map(i => {
            val partId = i._2 + machineId * localScaleFactor
            // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
            // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
            val part = new BSPModel.Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = partId
                val topo = i._1
                val members = i._1.vertices.map(j => cells(j)).toList
            }
            new partActor(BSPModel.Optimize.default(part))
        }).seq.toVector
    }
}