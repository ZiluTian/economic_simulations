package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._
import BSPModel.Connector._
import BSPModel.example.epidemics._

class handOptERMTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "ERM (graph-centric)"

    test(f"The hand-optimized $experimentName example"){
        List(1000).foreach(population => {
            // writer.write(f"Config: population ${population} rounds ${totalRounds} ")

            val graph = toGraphInt(GraphFactory.erdosRenyi(population, connectivity).adjacencyList())
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

            var readWrite = readOnly.map(_.clone)

            benchmarkTool[Unit]({
                Range(0, totalRounds).foreach(_ => {
                    // val summary = readOnly.map(i => i.asInstanceOf[PersonCell]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    // println(f"Summary: ${summary}")

                    readOnly.zipWithIndex.foreach(pair => {
                        val person = pair._1
                        var health = person.health 
                        if (health != SIRModel.Deceased) {                            
                            // println(f"${pair._2} person before computing has state ${pair._1}")
                            // println(f"${pair._2} person receives values ${person.neighbors.map(i => readOnly(i).risk)}")
                            health = person.neighbors.map(i => readOnly(i)).foldLeft(health)((x, y) => {
                                var personalRisk = y.risk
                                if (person.age > 60) {
                                    personalRisk = 2 * personalRisk
                                }
                                if (personalRisk > 1) {
                                    SIRModel.change(health, person.vulnerability)
                                } else {
                                    health
                                }
                            })

                            // The following code assumes the person meets with neighbors one by one
                            // person.neighbors.foreach(i => {
                            //     var personalRisk = readOnly(i).risk 
                            //     if (person.age > 60) {
                            //         personalRisk = personalRisk * 2
                            //     }
                            //     if (personalRisk > 1) {
                            //         health = SIRModel.change(health, person.vulnerability)
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