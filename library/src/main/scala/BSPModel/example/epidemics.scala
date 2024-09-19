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
                val vulnerability: Int, 
                var daysInfected: Int)

class PersonAgent(pos: BSPId, initHealth: Int, neighbors: Seq[BSPId]) extends BSP with ComputeMethod {
    type State = Person
    type InMessage = Double // risk of being infected
    type OutMessage = Double

    val age: Int = Random.nextInt(90)+10
    var state: Person = Person(age, 
            Random.nextBoolean(), 
            initHealth,
            vulnerability = if (age > 60) 1 else 0,
            0)
    override val id = pos
    val receiveFrom = FixedCommunication(neighbors) 

    // A helper method
    def partialComputeStateful(ms: Iterable[Double], s: Person): Person = {
        println(f"Partial compute stateful is invoked with ${ms}")
        ms match {
            case Nil => 
            case _ => 
                ms.foreach(risk => {
                    var personalRisk = risk
                    if (s.age > 60) {
                        personalRisk = personalRisk * 2
                    }
                    if (personalRisk > 1) {
                        s.health = SIRModel.change(s.health, s.vulnerability)
                    }
                })
        }
        s
    }

    def partialCompute(ms: Iterable[Double]): Option[Double] = {
        println(f"Partial compute is invoked with ${ms}")
        val stackTrace = Thread.currentThread().getStackTrace
        if (stackTrace.length > 3) {
            val callerElement = stackTrace(3)  // The immediate caller
            // println(s"Called from method: ${callerMethod.getMethodName}")
            // Extract detailed information
            val fullClassName = callerElement.getClassName  // full class name including package
            val methodName = callerElement.getMethodName    // method name
            val fileName = callerElement.getFileName        // file name where the method is defined
            val lineNumber = callerElement.getLineNumber    // line number in the file

            println(s"Called from method: $methodName in class: $fullClassName (file: $fileName, line: $lineNumber)")

        } else {
            println("Unable to trace the caller")
        }
        
        state = partialComputeStateful(ms, state)
        None
    }
    
    def updateState(person: Person, m: Option[Double]): Person = {
        println(f"Update state is invoked with ${m}")
        if (person.health != SIRModel.Deceased) {
            // person.health = state.health
            m match {
                case None => 
                case Some(risk) => 
                    println("This should not be printed!")
                    partialCompute(List(risk))
                }

            // person.health = state.health
            if ((person.health != SIRModel.Susceptible) && (person.health != SIRModel.Recover)) {
                if (person.daysInfected >= SIRModel.stateDuration(person.health)) {            
                    person.health = SIRModel.change(person.health, person.vulnerability)  
                } else {
                    person.daysInfected = person.daysInfected + 1
                }
            } 
        } 
        // state.health = person.health
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

// trait ERMCompute extends ComputeMethod {
//     type State = Person
//     type Message = Double // risk of being infected
//     type OutMessage = Double
    
//     def partialCompute(m1: Iterable[List[Double]]): Option[List[Double]] = {
//         m1 match {
//             case Nil => None
//             case _ => Some(m1.toList.flatten)
//         }
//     }

//     def updateState(person: Person, m: Option[List[Double]]): Person = {
//         if (person.health != SIRModel.Deceased) {
//             var health: Int = person.health
//             m match {
//                 case None => 
//                 case Some(risks) => 
//                     risks.foreach(risk => {
//                         var personalRisk = stateToMessage(person).head
//                         if (person.age > 60) {
//                             personalRisk = personalRisk * 2
//                         }
//                         if (personalRisk > 1) {
//                             health = SIRModel.change(health, person.vulnerability)
//                         }
//                     })
//                 }

//             if ((health != SIRModel.Susceptible) && (health != SIRModel.Recover)) {
//                 if (person.daysInfected >= SIRModel.stateDuration(health)) {                        
//                     Person(person.age, person.symptomatic, SIRModel.change(health, person.vulnerability), person.vulnerability, 0)
//                 } else {
//                     Person(person.age, person.symptomatic, health, person.vulnerability, person.daysInfected + 1)
//                 }
//             } else {
//                 person
//             }
//         } else {
//             person
//         }
//     }

//     def stateToMessage(s: Person): List[Double] = {
//         if (s.health == SIRModel.Infectious) {
//             List(SIRModel.infectiousness(s.health, s.symptomatic))
//         } else {
//             List(0)
//         }
//     }
// }

// class PersonAgent(pos: BSPId, initHealth: Int, neighbors: Seq[BSPId]) extends BSP with StatefulComputeMethod {
//     type State = Person
//     type InMessage = Double // risk of being infected
//     type OutMessage = Double

//     val age: Int = Random.nextInt(90)+10
//     var state: Person = Person(age, 
//             Random.nextBoolean(), 
//             initHealth,
//             vulnerability = if (age > 60) 1 else 0,
//             0)
//     override val id = pos
//     val receiveFrom = FixedCommunication(neighbors) 

//     def statefulFold(ms: Iterable[Double]): Unit = {
//         ms match {
//             case Nil => 
//             case _ => 
//                 ms.foreach(risk => {
//                     var personalRisk = stateToMessage(state)
//                     if (state.age > 60) {
//                         personalRisk = personalRisk * 2
//                     }
//                     if (personalRisk > 1) {
//                         state.health = SIRModel.change(state.health, state.vulnerability)
//                     }
//                 })
//         }
//         None
//     }

//     // expression for updating the state, NOT in-place update
//     def updateState(person: Person, m: Option[Double]): Person = {
//         if (person.health != SIRModel.Deceased) {
//             m match {
//                 case None => 
//                 case Some(risk) => 
//                     statefulFold(List(risk))
//                 }

//             person.health = state.health
//             if ((person.health != SIRModel.Susceptible) && (person.health != SIRModel.Recover)) {
//                 if (person.daysInfected >= SIRModel.stateDuration(person.health)) {            
//                     person.health = SIRModel.change(person.health, person.vulnerability)  
//                 } else {
//                     person.daysInfected = person.daysInfected + 1
//                 }
//             } 
//         } 
//         state.health = person.health
//         person
//     }

//     def stateToMessage(s: Person): Double = {
//         if (s.health == SIRModel.Infectious) {
//             SIRModel.infectiousness(s.health, s.symptomatic)
//         } else {
//             0
//         }
//     }
// } 

