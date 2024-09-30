package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{DoubleVectorMessage, Actor}
import BSPModel.example.epidemics._
import BSPModel.Connector._
import scala.collection.mutable.{Map => MutMap}

abstract class EpidemicsGraphScaleOutTest extends scaleOutTest {

    class partActor(partId: Int, inExtVertices: Map[Int, Vector[Int]], outIntVertices: Map[Int, Vector[Int]], people: Map[Int, PersonCell]) extends Actor {
        id = partId.toLong
        val receivedValues: MutMap[BSPId, Double] = MutMap[BSPId, Double]()
        // Cache only the risk attribute, in-place update other attributes only the fly
        var risks: Map[BSPId, Double] = people.mapValues(i => i.risk)
        
        override def run(): Int = {
            receivedMessages.foreach(i => {
                val rpart = i.asInstanceOf[DoubleVectorMessage].value(0).toInt
                // println(f"$partId receives from id ${rpart} value ${i.asInstanceOf[DoubleVectorMessage].value.tail}")
                i.asInstanceOf[DoubleVectorMessage].value.tail.zipWithIndex.foreach(j => {
                    receivedValues.update(inExtVertices(rpart)(j._2), j._1)
                })
            })
            receivedMessages.clear()
            
            people.foreach(pair => {
                val person = pair._2
                var health = person.health 
                if (health != SIRModel.Deceased) {
                    person.neighbors.foreach(i => {
                        var personalRisk = if (people.isDefinedAt(i)) {
                            risks(i)    // read from cached value, not immediately updated values
                        } else if (receivedValues.isDefinedAt(i)) {
                            receivedValues(i)
                        } else {
                            0
                        } 
                        if (person.age > 60) {
                            personalRisk = personalRisk * 2
                        }
                        if (personalRisk > 1) {
                            health = SIRModel.change(health, person.vulnerability)
                        }
                    })
                    
                    // .foldLeft(health)((x, y) => {
                    //     var personalRisk = y
                    //     if (person.age > 60) {
                    //         personalRisk = 2 * personalRisk
                    //     }
                    //     if (personalRisk > 1) {
                    //         SIRModel.change(health, person.vulnerability)
                    //     } else {
                    //         health
                    //     }
                    // })

                    if (health == SIRModel.Infectious) {
                        person.risk = SIRModel.infectiousness(health, person.symptomatic)
                    }

                    if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                        if (person.daysInfected >= SIRModel.stateDuration(health)) {
                            health = SIRModel.change(health, person.vulnerability)
                            person.daysInfected = 0
                        } else {
                            person.daysInfected += 1
                        }
                    }
                    person.health = health
                } 
            })
            risks = people.mapValues(i => i.risk)

            outIntVertices.foreach(i => {
                val message = DoubleVectorMessage(partId.toDouble +: i._2.map(j => risks(j)).toVector)
                sendMessage(i._1, message)
            })
            1
        }
    }

    def genPopulationHashed(g: Map[Int, Iterable[Int]], partitionSize: Int): Map[Int, List[(Int, PersonCell)]] = {
        var ans = Map[Int, List[(Int, PersonCell)]]()
        g.foreach(i => {
            val age: Int = Random.nextInt(90)+10
            val health: Int = if (Random.nextInt(100)==0) 0 else 2
            val symptomatic: Boolean = Random.nextBoolean()
            val v = PersonCell(age, 
                i._2,
                symptomatic, 
                health,
                vulnerability = if (age > 60) 1 else 0,
                0,
                if (health == SIRModel.Infectious) {
                    SIRModel.infectiousness(health, symptomatic)
                } else {
                    0
                })
            ans = ans + ((i._1 / partitionSize) -> ((i._1, v) :: ans.getOrElse(i._1 / partitionSize, List[(Int, PersonCell)]())))
        })
        ans
    }

    def genPopulation(g: Map[Int, Iterable[Int]]): Map[Int, PersonCell] = {
        g.map(i => {
            val age: Int = Random.nextInt(90)+10
            val health: Int = if (Random.nextInt(100)==0) 0 else 2
            val symptomatic: Boolean = Random.nextBoolean()
            (i._1, PersonCell(age, 
                i._2,
                symptomatic, 
                health,
                vulnerability = if (age > 60) 1 else 0,
                0,
                if (health == SIRModel.Infectious) {
                    SIRModel.infectiousness(health, symptomatic)
                } else {
                    0
                }))
        })
    }
}

object ERMGraphScaleOutTest extends EpidemicsGraphScaleOutTest with App {
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
        val crossPartitionEdges = cuts(partitionSize, localScaleFactor * totalMachines)
        println(f"Cross partition edges on $machineId are have been computed!")
        var graph: Map[Int, Iterable[Int]] =
            toGraphInt(GraphFactory.erdosRenyi(baseFactor, p, startingIndex ).adjacencyList)
            .map(i => (i._1, crossPartitionEdges.getOrElse(i._1, Set()) ++ i._2))
        println(f"Graph at $machineId has been constructed!")
        
        val cells: Map[Int, List[(Int, PersonCell)]] = genPopulationHashed(graph, partitionSize)
        val edges: Iterable[(Int, Int)] = graph.toIterable.flatMap { case (node, neighbors) =>
            neighbors.flatMap(neighbor => if ((neighbor / partitionSize) != (node / partitionSize)) List((node, neighbor), (neighbor, node)) else List())
        }.toSet

        val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)

        partitionPartialGraph(edges, partIds, partitionSize).view.zipWithIndex.map(i => {
            new partActor(partIds(i._2), i._1.inExtVertices, i._1.outIntVertices, cells.getOrElse(machineId * localScaleFactor + i._2, List()).toMap)
        }).toVector
    }
}

object SBMGraphScaleOutTest extends  EpidemicsGraphScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val q: Double = 0
        val startingIndex = machineId * baseFactor
        val partitionSize = baseFactor / localScaleFactor
        val graph = GraphFactory.stochasticBlock(baseFactor, p, q, 5, startingIndex)
        // val graph = GraphFactory.erdosRenyi(baseFactor, p, startingIndex)
        // val cells: Map[Int, PersonCell] = genPopulation(toGraphInt(graph.adjacencyList))

        val cells: Map[Int, List[(Int, PersonCell)]] = genPopulationHashed(toGraphInt(graph.adjacencyList), partitionSize)
        val edges: Iterable[(BSPId, BSPId)] = graph.edges.filter { case (node, neighbor) =>
            neighbor / partitionSize != node / partitionSize
        }.toSet
        val partIds = (0 until localScaleFactor).map(i => localScaleFactor * machineId + i)
        partitionPartialGraph(edges, partIds, partitionSize).view.zipWithIndex.map(i => {
            new partActor(partIds(i._2), i._1.inExtVertices, i._1.outIntVertices, cells(machineId * localScaleFactor + i._2).toMap)
        }).toVector

        // partition(graph, localScaleFactor, localScaleFactor*machineId).view.zipWithIndex.map(i => {
        //     val partId = localScaleFactor * machineId + i._2
        //     // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
        //     // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
        //     new partActor(partId, i._1.inExtVertices, i._1.outIntVertices, cells.filter(j => i._1.vertices.contains(j._1)))
        // }).toVector
    }
}