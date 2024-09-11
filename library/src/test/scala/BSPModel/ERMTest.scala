package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._

class ERMTest extends BSPBenchSuite {

    val population: Int = 1000
    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "ERM (opt, stateless)"

    case class Person(val age: Int, 
                val symptomatic: Boolean,
                val health: Int,
                val vulnerability: Int, 
                val daysInfected: Int)

    trait ERMCompute extends ComputeMethod {
        type State = Person
        type Message = List[Double] // risk of being infected

        def partialCompute(m1: Iterable[List[Double]]): Option[List[Double]] = {
            m1 match {
                case Nil => None
                case _ => Some(m1.toList.flatten)
            }
        }

        def updateState(person: Person, m: Option[List[Double]]): Person = {
            if (person.health != SIRModel.Deceased) {
                var health: Int = person.health
                m match {
                    case None => 
                    case Some(risks) => 
                        risks.foreach(risk => {
                            var personalRisk = stateToMessage(person).head
                            if (person.age > 60) {
                                personalRisk = personalRisk * 2
                            }
                            if (personalRisk > 1) {
                                health = SIRModel.change(health, person.vulnerability)
                            }
                        })
                    }

                if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                    if (person.daysInfected >= SIRModel.stateDuration(health)) {                        
                        Person(person.age, person.symptomatic, SIRModel.change(health, person.vulnerability), person.vulnerability, 0)
                    } else {
                        Person(person.age, person.symptomatic, health, person.vulnerability, person.daysInfected + 1)
                    }
                } else {
                    person
                }
            } else {
                person
            }
        }

        def stateToMessage(s: Person): List[Double] = {
            if (s.health == SIRModel.Infectious) {
                List(SIRModel.infectiousness(s.health, s.symptomatic))
            } else {
                List(0)
            }
        }
    }

    class PersonAgent(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with ERMCompute {
        val age: Int = Random.nextInt(90)+10
        var state: Person = Person(age, 
                Random.nextBoolean(), 
                if (Random.nextInt(100)==0) 0 else 2,
                vulnerability = if (age > 60) 1 else 0,
                0)
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 
    }

    test(f"${experimentName} example should run") {
        val g: Map[Long, Iterable[Long]] = (new ErdosRenyiGraph(population, connectivity)).g
        val agents = g.map(i => new PersonAgent(i._1, i._2.toSeq))

        // binding information (partition structure)
        val initPartition = new Partition {
            type Member = BSP with ComputeMethod
            type NodeId = BSPId
            type Value = BSP
            val id = 1

            val topo = new BSPModel.Graph[BSPId]{
                val vertices = agents.map(a => a.id).toSet
                val edges = g.map(i => (i._1, i._2.toList))
                val inEdges = Map()
                val outEdges = Map()
            }

            val members = agents.toList
        }

        val ans = BSPModel.Optimize.default(initPartition)

        benchmarkTool[Unit](writer,
            Range(1, totalRounds).foreach(_ => {
                ans.members.map(i => {
                    i.run(List())
                    // println(i.toString)
                })
                // DoubleBufferToBSP and BSPToDoubleBuffer
                // val summary = ans.members.map(_.state.asInstanceOf[(Array[BSP with ComputeMethod with DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = BSPId}])]._1).flatMap(k => k.map(i => i.state.asInstanceOf[Person])).groupBy(i => i.health).map(i => (i._1, i._2.size))
                // BSPToDoubleBuffer
                // val summary = ans.members.map(_.state.asInstanceOf[Person]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                // println(f"Summary: ${summary}")
            })
        ) 
    }
}