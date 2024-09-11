package BSPModel
package test

import scala.math.max
import scala.util.Random
// Rand object is the implicit random generator for Gamma
import breeze.stats.distributions.{Rand, Gamma}

// Extended SIR model
object SIRModel {
    // Random seed
    val r_seed = new Random(100)

    // Encodings (health states)
    val Susceptible: Int = 0
    val Exposed: Int = 1
    val Infectious: Int = 2
    val Hospitalized: Int = 3
    val Recover: Int = 4
    val Deceased: Int = 5
    // Encodings (vulnerability levels)
    val Low: Int = 0
    val High: Int = 1
    // Infectious parameter (gamma distribution)
    val infectiousAlpha = 0.25
    val infectiousBeta = 1
    val symptomaticSkew = 2

    var avgIDuration: Int = 5
    var avgHDuration: Int = 7
    var avgEDuration: Int = 5

    var probLE2I: Double = 0.6
    var probLI2H: Double = 0.1
    var probLH2D: Double = 0.1
    var probLDefault: Double = 0.01

    var probHE2I: Double = 0.9
    var probHI2H: Double = 0.4
    var probHH2D: Double = 0.5
    var probHDefault: Double = 0.05

    def change(health: Int, vulnerability: Int): Int = {
        health match {
            case Susceptible => Exposed
            case Exposed => 
                val worse_prob: Double = eval(vulnerability, Exposed, Infectious)
                if (r_seed.nextDouble < worse_prob) {
                    Infectious
                } else {
                    Recover
                }
            case Infectious => 
                val worse_prob: Double = eval(vulnerability, Infectious, Hospitalized)
                if (r_seed.nextDouble < worse_prob) {
                    Hospitalized
                } else {
                    Recover
                }
            case Hospitalized =>
                val worse_prob: Double = eval(vulnerability, Hospitalized, Deceased)
                if (r_seed.nextDouble < worse_prob) {
                    Deceased
                } else {
                    Recover
                }
            case _ => health
        }
    }

    def stateDuration(health: Int): Int = {
        health match {
            case Infectious => 
                // 3 + Random.nextInt(6)
                max(1, avgIDuration + r_seed.nextGaussian().toInt) 
            case Hospitalized => 
                // 2 + Random.nextInt(10)
                max(1, avgHDuration + r_seed.nextGaussian().toInt) 
            case Exposed => 
                // 3 + Random.nextInt(5)
                max(1, avgEDuration + r_seed.nextGaussian().toInt)
            case _ => Int.MaxValue
        }
    }

    def infectiousness(health: Int, symptomatic: Boolean): Double = {
        if (health == Infectious) {
            var gd = Gamma(infectiousAlpha, infectiousBeta)(Rand).draw()
            if (symptomatic){
                gd = gd * 2
            }
            gd
        } else {
            0
        }
    }

    def eval(vulnerability: Int, currentHealth: Int, projectedHealth: Int): Double = {
        vulnerability match {
            case Low =>
                (currentHealth, projectedHealth) match {
                    case (Exposed, Infectious) => probLE2I
                    case (Infectious, Hospitalized) => probLI2H
                    case (Hospitalized, Deceased) => probLH2D
                    case _ => probLDefault
                }
            case High =>
                (currentHealth, projectedHealth) match {
                    case (Exposed, Infectious) => probHE2I
                    case (Infectious, Hospitalized) => probHI2H
                    case (Hospitalized, Deceased) => probHH2D
                    case _ => probHDefault
                }
        }
    }
}