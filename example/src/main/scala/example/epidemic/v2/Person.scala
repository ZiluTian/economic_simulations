package example
package epidemic
package v2

import scala.util.Random
import meta.classLifting.SpecialInstructions._
import squid.quasi.lift
import meta.runtime.{Message, DoubleMessage}

@lift
class Person(val age: Int) extends Actor {
    // Move all local vars up to class variables
    val symptomatic: Boolean = Random.nextBoolean()
    var health: Int = 0
    var vulnerability: Int = 0
    var daysInfected: Int = 0
    var m: Option[Message] = None
    var selfRisk: Double = 0

    def main(): Unit = {
        vulnerability = if (age > 60) 1 else 0
        if (Random.nextInt(100)==0){
            health = 2
        }

        while (true) {
            if (health != SIRModel.Deceased) {
                m = receiveMessage()
                while (m.isDefined){
                    if (health == 0) {
                        var personalRisk = m.get.asInstanceOf[DoubleMessage].value
                        if (age > 60) {
                            personalRisk = personalRisk * 2
                        }
                        if (personalRisk > 1) {
                            health = SIRModel.change(health, vulnerability)
                        }
                    }
                    m = receiveMessage()
                }

                // Meet with contacts 
                if (health == SIRModel.Infectious) {
                    selfRisk = SIRModel.infectiousness(health, symptomatic)
                    val msg = new DoubleMessage(selfRisk)
                    connectedAgentIds.foreach(i => {
                        sendMessage(i, msg)
                    })
                }

                if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
                    if (daysInfected == SIRModel.stateDuration(health)) {
                        // health = 4
                        health = SIRModel.change(health, vulnerability)
                        daysInfected = 0
                    } else {
                        daysInfected = daysInfected + 1
                    }
                }
            } 
            waitRounds(1)
        }
    }
}