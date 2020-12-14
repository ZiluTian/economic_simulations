package example.epistemicLogicVC

import lib.EpistemicLogic.Sentence._
import meta.classLifting.SpecialInstructions._
import meta.deep.runtime.Actor
import meta.deep.runtime.Actor.AgentId

import scala.collection.mutable.ListBuffer
import squid.quasi.lift
import VCHelper._

import scala.util.Random

@lift
class Process extends Actor {
  var vectorClock: VectorClock = new VectorClock()

  var localTime: Int = 0
  val initTime: Int = 0

  var others: List[Process] = Nil
  var neighborIds: List[AgentId] = Nil

  def init(): Unit = {
    val nIds: ListBuffer[AgentId] = new ListBuffer[AgentId]()
    others.foreach(i => {
      vectorClock.recordLearning(localTime, Set(P(ProcessTime(i.id, initTime))))
      nIds.append(i.id)
    })
    neighborIds = nIds.toList
    vectorClock.recordLearning(localTime, Set(P(ProcessTime(id, localTime))))
  }

  def incLocalTime(): Unit = {
    // retrieve the current time and replace it with inc one
    val currState: EpistemicSentence = P(ProcessTime(id, localTime))
    if (vectorClock.know(currState)) {
      vectorClock.forget(Set(currState))
      localTime = localTime + 1
      val newState = P(ProcessTime(id, localTime))
      vectorClock.recordLearning(localTime, Set(newState))
    } else {
      println("Error at increasing local time!")
    }
  }

  def internalEvent(): Unit = {
    println("Agent " + id + " internal action!")
    incLocalTime()
  }

  // send a message to another replica
  def send(another: Process): Unit = {
    incLocalTime()
    val copy: Set[EpistemicSentence] = vectorClock.getKnowledgeBase
    asyncMessage(() =>
      another.receive(copy))
  }

  def receive(cvec: Set[EpistemicSentence]): Unit = {
    println("Agent " + id + " received a message! " + cvec)
    println("Current knowledge base: " + vectorClock.knowledgeBase)
    incLocalTime()
    vectorClock.learn(cvec).toList.foreach(v => {
      vectorClock.updateTimingInfo(v)
    })
    println("Updated knowledge base: " + vectorClock.knowledgeBase)
  }

  def main(): Unit = {
    init()
    while (true) {
      val r: Int = Random.nextInt(others.length+1)
      if (r == others.length) {
        internalEvent()
      } else {
        send(others(r))
      }
      handleMessages()
      waitTurns(1)
    }
  }
}