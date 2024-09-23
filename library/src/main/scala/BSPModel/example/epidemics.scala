package BSPModel
package example
package epidemics

import scala.util.Random

// PersonCell, needs to carry info like neighbors and risk, which are encoded in BSP
case class PersonCell(val age: Int, 
                val neighbors: Iterable[Int], 
                val symptomatic: Boolean,
                var health: Int,
                var vulnerability: Int, 
                var daysInfected: Int, 
                var risk: Double) {
    override def clone(): PersonCell = {
        new PersonCell(this.age, this.neighbors, this.symptomatic, this.health, this.vulnerability, this.daysInfected, this.risk)
    } 
}

case class Person(val age: Int, 
                val symptomatic: Boolean,
                var health: Int,
                var vulnerability: Int, 
                var daysInfected: Int)

class PersonAgent(pos: BSPId, 
        age: Int, 
        neighbors: Seq[BSPId], 
        symptomatic: Boolean, 
        health: Int, 
        vulnerability: Int) extends BSP with ComputeMethod {
    type State = Person
    type InMessage = Double // risk of being infected
    type SerializeFormat = Double

    var state: Person = Person(age, 
            symptomatic, 
            health,
            vulnerability,
            0)
    
    override val id = pos
    val receiveFrom = FixedCommunication(neighbors) 

    def updateState(person: Person, m: Option[Double]): Person = {
        var health = person.health
        // println(f"$pos update state is invoked with ${m}")
        if (health != SIRModel.Deceased) {
            m match {
                case None => 
                case Some(h) => 
                    health = h.toInt
                }

            if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                if (person.daysInfected >= SIRModel.stateDuration(health)) {            
                    health = SIRModel.change(health, person.vulnerability)  
                    person.daysInfected = 0
                } else {
                    person.daysInfected = person.daysInfected + 1
                }
            }  
            person.health = health
        } 
        person
    }

    // When processing messages, assume that the person immediately contacts with all (the same health state while bulk processing)
    def partialCompute(ms: Iterable[Double]): Option[Double] = {
        // println(f"$pos before partial compute is invoked with ${ms} state ${state}")
        var health = state.health
        ms.foldLeft(health)((x, y) => {
            var personalRisk = y
            if (state.age > 60) {
                personalRisk = 2 * personalRisk
            }
            if (personalRisk > 1) {
                SIRModel.change(health, state.vulnerability)
            } else {
                health
            }
        })
        // ms.foreach(risk => {
        //     var personalRisk = risk
        //     if (state.age > 60) {
        //         personalRisk = personalRisk * 2
        //     }
        //     if (personalRisk > 1) {
        //         health = SIRModel.change(health, state.vulnerability)
        //     }
        // })
        Some(health.toDouble)
    }

    def stateToMessage(person: Person): Double = {
        if (person.health == SIRModel.Infectious) {
            SIRModel.infectiousness(person.health, person.symptomatic)
        } else {
            0
        }
    }
} 