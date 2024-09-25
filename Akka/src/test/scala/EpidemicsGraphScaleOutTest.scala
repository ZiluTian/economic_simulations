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

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val startingIndex = machineId * baseFactor
        // val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)

        // Generate a partial graph
        val adjList = (startingIndex until (startingIndex + baseFactor)).map { i =>
            val neighbors = (0 until (startingIndex + baseFactor * totalMachines)).filter(j => (i != j) && (Random.nextDouble() < p))
            (i -> neighbors)
        }.toMap

        val cells = genPopulation(adjList)

        partitionPartialGraph(adjList.toIterable.flatMap(i => i._2.map(j => (j, i._1))), adjList.keys.toSet, (0 until totalMachines * baseFactor).map(i => (i, i % baseFactor)).toMap).view.zipWithIndex.map(i => 
            new partActor(i._2, i._1.inExtVertices, i._1.outIntVertices, i._1.vertices.map(j => (j, cells(j))).toMap)
        ).toVector
    }
}

object SBMGraphScaleOutTest extends  EpidemicsGraphScaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val startingIndex = machineId * baseFactor
        // val graph = GraphFactory.erdosRenyi(baseFactor * scaleUpFactor, p)

        // Generate a partial graph
        val adjList = (startingIndex until (startingIndex + baseFactor)).map { i =>
            val neighbors = (startingIndex until (startingIndex + baseFactor)).filter(j => (i != j) && (Random.nextDouble() < p))
            (i -> neighbors)
        }.toMap

        val cells = genPopulation(adjList)

        partitionPartialGraph(adjList.toIterable.flatMap(i => i._2.map(j => (j, i._1))), adjList.keys.toSet, (0 until totalMachines * baseFactor).map(i => (i, i % baseFactor)).toMap).view.zipWithIndex.map(i => 
            new partActor(i._2, i._1.inExtVertices, i._1.outIntVertices, i._1.vertices.map(j => (j, cells(j))).toMap)
        ).toVector
    }
}