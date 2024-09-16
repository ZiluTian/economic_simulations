package BSPModel
package test

import scala.util.Random
import cloudcity.lib.Graph._

class SBMStatefulTest extends BSPBenchSuite {

    val connectivity: Double = 0.01
    val totalRounds: Int = 50
    val experimentName: String = "SBM (opt)"

    case class Person(val age: Int, 
                val symptomatic: Boolean,
                var health: Int,
                val vulnerability: Int, 
                var daysInfected: Int)

    class PersonAgent(pos: BSPId, initHealth: Int, neighbors: Seq[BSPId]) extends BSP with StatefulComputeMethod {
        val age: Int = Random.nextInt(90)+10
        var state: Person = Person(age, 
                Random.nextBoolean(), 
                initHealth,
                vulnerability = if (age > 60) 1 else 0,
                0)
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 

        type State = Person
        type Message = Double // risk of being infected

        // var partiallyComputedState: Int = initHealth

        // in-place update
        def statefulFold(ms: Iterable[Double]): Unit = {
            // println(f"Messages received are ${m1}")
            ms match {
                case Nil => 
                case _ => 
                    ms.foreach(risk => {
                        var personalRisk = stateToMessage(state)
                        if (state.age > 60) {
                            personalRisk = personalRisk * 2
                        }
                        if (personalRisk > 1) {
                            state.health = SIRModel.change(state.health, state.vulnerability)
                        }
                    })
            }
            None
        }

        // expression for updating the state, NOT in-place update
        def updateState(person: Person, m: Option[Double]): Person = {
            if (person.health != SIRModel.Deceased) {
                // merge partiallyComputedState (staged) 
                m match {
                    case None => 
                    case Some(risk) => 
                        statefulFold(List(risk))
                    }

                if ((person.health != SIRModel.Susceptible) && (person.health != SIRModel.Recover)) {
                    if (person.daysInfected >= SIRModel.stateDuration(person.health)) {            
                        person.health = SIRModel.change(person.health, person.vulnerability)  
                    } else {
                        person.daysInfected = person.daysInfected + 1
                    }
                } 
            } 
            person
        }

        def stateToMessage(s: Person): Double = {
            if (s.health == SIRModel.Infectious) {
                SIRModel.infectiousness(s.health, s.symptomatic)
            } else {
                0
            }
        }
    } 

    test(f"${experimentName} example should run") {
        List(1000, 10000, 100000).foreach(population => {
            writer.write(f"Config: population ${population} rounds ${totalRounds}\n")
            val graph = GraphFactory.stochasticBlock(population, connectivity, 0, 5)
            val agents = toGraphInt(graph.adjacencyList()).map(i => new PersonAgent(i._1.toInt, if (Random.nextInt(100)==0) 0 else 2, i._2.toSeq))

            // binding information (partition structure)
            val initPartition = new Partition {
                type Member = BSP with ComputeMethod
                type NodeId = BSPId
                type Value = BSP
                val id = 1

                val topo = new BSPModel.Graph[BSPId]{
                    val vertices = graph.nodes
                    val edges = graph.adjacencyList()
                    val inExtVertices = Map()
                    val outIntVertices = Map()
                }

                val members = agents.toList
            }

            val ans = BSPModel.Optimize.default(initPartition)

            benchmarkTool[Unit]( 
                writer, 
                Range(1, totalRounds).foreach(_ => {
                    ans.members.map(i => {
                        i.run(List())
                    })
                    // val summary = ans.members.map(_.state.asInstanceOf[Person]).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    // val summary = ans.members.map(_.state.asInstanceOf[(Array[BSP with ComputeMethod with DoubleBuffer], Option[PartitionMessage{type M = BSP; type Idx = BSPId}])]._1).flatMap(k => k.map(i => i.state.asInstanceOf[Person])).groupBy(i => i.health).map(i => (i._1, i._2.size))
                    // println(f"Summary: ${summary}")
                })
            ) 
        })
    }
}