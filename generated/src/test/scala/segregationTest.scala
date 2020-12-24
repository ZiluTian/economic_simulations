package generated.example
package test

class segregationTest extends FlatSpec {

  import segregation._
  import ch.qos.logback.classic.Level._

  "The similarity level of the segregation model" should "converge to around 0.85" in {
    val totalTurns: Int = 70
    val Sim = new Simulation(new Config(InitData.initActors, 0, totalTurns, 0, 0))
    val last10 = recordLogEvents.record({
      Sim.run()
    }).filter(e => e.getLevel().equals(INFO)).map(i => i.getFormattedMessage.stripMargin.toDouble).drop(totalTurns-10)
    assert((last10.sum / last10.length - 0.85).abs < 0.03)
  }

//  "The similarity level of the segregation model" should "converge to around 0.85, Spark" in {
//    val totalTurns: Int = 70
//    val last10 = recordLogEvents.record({
//      SimulationSpark.run(new Config(segregation.InitData.initActors, 0, totalTurns, 0, 0))
//    }).filter(e => e.getLevel().equals(INFO)).map(i => i.getFormattedMessage.stripMargin.toDouble).drop(totalTurns-10)
//    assert((last10.sum / last10.length - 0.85).abs < 0.03)
//  }
}
