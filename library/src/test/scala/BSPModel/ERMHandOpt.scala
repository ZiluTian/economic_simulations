package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._

class handOptERMTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "ERM (graph-centric)"
    
    case class Person(val age: Int, 
                    val neighbors: Iterable[Int], 
                    val symptomatic: Boolean,
                    var health: Int,
                    var vulnerability: Int, 
                    var daysInfected: Int, 
                    var risk: Double) 

    test(f"The hand-optimized $experimentName example"){
        List(10000).foreach(population => {
            writer.write(f"Config: population ${population} rounds ${totalRounds} ")

            val graph = toGraphInt(GraphFactory.erdosRenyi(population, connectivity).adjacencyList())
            var readOnly: Array[Person] = (0 until population).map(i => {
                val age: Int = Random.nextInt(90)+10
                Person(age, 
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