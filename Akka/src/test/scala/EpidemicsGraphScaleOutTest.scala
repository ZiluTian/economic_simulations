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

        // println(f"Partition ${partId} incoming external vertices are ${inExtVertices}")
        // println(f"Partition ${partId} outgoing internal vertices are ${outIntVertices}")
        
        override def run(): Int = {
            receivedMessages.foreach(i => {
                val rpart = i.asInstanceOf[DoubleVectorMessage].value(0).toInt
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
                        } else if (time == 0) {
                            // in the first round, no message is received
                            0
                        } else {
                            throw new Exception(f"Neighbor ${i} not found in local or remote in partition ${partId}!")
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

    def genPopulation(g: Map[Int, Iterable[Int]]): Map[Int, PersonCell] = {
        g.mapValues(i => {
            val age: Int = Random.nextInt(90)+10
            val health: Int = if (Random.nextInt(100)==0) 0 else 2
            val symptomatic: Boolean = Random.nextBoolean()
            PersonCell(age, 
                i,
                symptomatic, 
                health,
                vulnerability = if (age > 60) 1 else 0,
                0,
                if (health == SIRModel.Infectious) {
                    SIRModel.infectiousness(health, symptomatic)
                } else {
                    0
                })
        })
    }
}

object ERMGraphScaleOutTest extends EpidemicsGraphScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def cuts(partitionSize: Int, totalPartitions: Int): Map[BSPId, Set[BSPId]] = {
        val p: Double = 0.01
        val rand = new Random(100)
        var graph = Map.empty[BSPId, Set[BSPId]]
        (0 until totalPartitions).foreach ({ i => 
            (0 until partitionSize).foreach(v => {
                val neighbors = ((i + 1) * partitionSize until totalPartitions * partitionSize).filter(_ => rand.nextDouble() < p)
                graph = graph + ((i * partitionSize + v) -> (graph.getOrElse(i * partitionSize + v, Set()) ++ neighbors))
                neighbors.foreach(n => {
                    graph = graph + (n -> (graph.getOrElse(n, Set()) + (i * partitionSize + v)))
                })
            })
        })
        graph
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val localScaleFactor: Int = 50
        val startingIndex = machineId * baseFactor
        val crossPartitionEdges = cuts(baseFactor / localScaleFactor, totalMachines * localScaleFactor)
        val graph: Map[BSPId, Iterable[BSPId]] = (0 until localScaleFactor).par.map(i => {
            toGraphInt(GraphFactory.erdosRenyi(baseFactor / localScaleFactor, p, startingIndex + i * (baseFactor / localScaleFactor)).adjacencyList)
        }).reduce(_++_).map(i => {
            (i._1, crossPartitionEdges.getOrElse(i._1, Set()) ++ i._2)
        })
        val cells: Map[Int, PersonCell] = genPopulation(graph)
        val partGraph: Map[Long, Iterable[Long]] = graph.map(i => (i._1.toLong, i._2.map(_.toLong)))
        (0 until localScaleFactor).map(i => { 
            val g = partitionPartialGraph(partGraph.toIterable.flatMap { case (node, neighbors) =>
                neighbors.map(neighbor => (node, neighbor))
            }, machineId * localScaleFactor + i, baseFactor / localScaleFactor)
            new partActor(localScaleFactor * machineId + i, g.inExtVertices, g.outIntVertices, g.vertices.map(j => (j, cells(j))).toMap)
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
        val localScaleFactor: Int = 50
        val startingIndex = machineId * baseFactor
        val graph = GraphFactory.erdosRenyi(baseFactor, p, startingIndex)
        val cells: Map[Int, PersonCell] = genPopulation(toGraphInt(graph.adjacencyList))

        partition(graph, localScaleFactor).zipWithIndex.map(i => {
            new partActor(localScaleFactor * machineId + i._2, i._1.inExtVertices, i._1.outIntVertices, i._1.vertices.map(j => (j, cells(j))).toMap)
        }).toVector
    }
}