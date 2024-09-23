package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._
import BSPModel.Connector._
import BSPModel.example.epidemics._

class handOptSBMTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "SBM (graph-centric)"
    
    test(f"The hand-optimized $experimentName example"){
        List(1000, 10000, 100000).foreach(population => {
            writer.write(f"Config: population ${population} rounds ${totalRounds}\n")

            val graph = toGraphInt(GraphFactory.stochasticBlock(population, connectivity, 0, 5).adjacencyList())
            var readOnly: Array[PersonCell] = (0 until population).map(i => {
                val age: Int = Random.nextInt(90)+10
                PersonCell(age, 
                    graph.getOrElse(i, List()), 
                    Random.nextBoolean(), 
                    if (Random.nextInt(100)==0) 0 else 2,
                    vulnerability = if (age > 60) 1 else 0,
                    0,
                    0)
            }).toArray

            var readWrite = readOnly.clone

            benchmarkTool[Unit](writer, {
                Range(1, totalRounds).foreach(_ => {
                    readOnly.zipWithIndex.foreach(pair => {
                        val person = pair._1
                        var health = person.health 
                        if (health != SIRModel.Deceased) {
                            // use reduce instead of combine, to allow for stateful updates, where the 
                            // initial value passed to reduce is stateful
                            person.neighbors.foreach(i => {
                                var personalRisk = readOnly(i).risk 
                                if (person.age > 60) {
                                    personalRisk = personalRisk * 2
                                }
                                if (personalRisk > 1) {
                                    health = SIRModel.change(health, person.vulnerability)
                                }
                            })
                                                        
                            // health = person.neighbors.view.map(i => readOnly(i).risk).foldLeft(health)((x, y) => {
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
                                readWrite(pair._2).risk = SIRModel.infectiousness(health, person.symptomatic)
                            }

                            if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                                if (person.daysInfected >= SIRModel.stateDuration(health)) {
                                    health = SIRModel.change(health, person.vulnerability)
                                    readWrite(pair._2).daysInfected = 0
                                } else {
                                    readWrite(pair._2).daysInfected += 1
                                }
                            }
                            readWrite(pair._2).health = health
                        } 
                    })
                    readOnly = readWrite.clone
                })
            })
    })
}}