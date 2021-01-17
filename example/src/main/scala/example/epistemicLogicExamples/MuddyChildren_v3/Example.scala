package example
package epistemicLogicExamples
package MuddyChildren_v3

import squid.quasi.lift

@lift
class MainInit {
  def main(): List[Actor] = {
    val l: ListBuffer[Actor] = new ListBuffer[Actor]()
    val lc: ListBuffer[Child] = new ListBuffer[Child]()

    val muddyChildren: Int = 4
    val totalChildren: Int = 7

    (1 to muddyChildren).foreach(_ =>
      lc.append(new Child(true))
    )

    (1 to (totalChildren - muddyChildren)).foreach(_ =>
      lc.append(new Child(false))
    )

    lc.foreach(c => {
      c.neighbors = lc.toList.filterNot(p => p==c)
    })

    l.appendAll(lc)

    l.toList
  }
}

object Example extends App {

  import lib.Bot.MessengerBot

  val cls1: ClassWithObject[Child] = Child.reflect(IR)
  val cls3: ClassWithObject[MessengerBot] = MessengerBot.reflect(IR)
  val mainClass: ClassWithObject[MainInit] = MainInit.reflect(IR)

  compileSims(List(cls1, cls3), Some(mainClass))
}