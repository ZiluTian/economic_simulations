package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import BSPModel.example.gameOfLife._
import scala.util.Random
import meta.runtime.{IntVectorMessage, IntArrayMessage, Actor}
import BSPModel.Connector._
import BSPModel.example.stockMarket._
import BSPModel.example.epidemics._

object gameOfLifeUnoptScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }
    
    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val totalHeight: Int = (baseFactor * totalMachines / width).toInt
        val height: Int = (baseFactor / width / localScaleFactor).toInt
        val startingIndex = machineId * baseFactor

        (startingIndex until startingIndex + baseFactor).map(index => {
            val x: Int = index % width
            val y: Int = index / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + totalHeight) % totalHeight
            } yield dy * width + dx
            bspToAgent(new Cell(index, neighbors.toSeq))
        })
    }
}

object stockMarketUnoptScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(machineId < totalMachines)

        val totalMarkets: Int = 1
        val totalTraders: Int = baseFactor * totalMachines - 1
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001

        if (machineId == 0) {
            val market = bspToAgent(new MarketAgent(0, (totalMarkets until (totalMarkets + totalTraders)).toVector, initialStockPrice))
            val traders = (1 until baseFactor).map(i => {
                bspToAgent(new TraderAgent(i, (0 until totalMarkets).toVector, budget, interestRate))
            })
            market.connectedAgentIds = (1L until baseFactor * totalMachines)
            Vector(market) ++ traders
        } else {
            ((baseFactor * machineId) until (baseFactor * (machineId + 1))).map(i => 
                bspToAgent(new TraderAgent(i, (0 until totalMarkets).toVector, budget, interestRate))
            )
        }
    }
}

object ERMUnoptScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(machineId < totalMachines)
        val p: Double = 0.01

        (0 until baseFactor).map(i => {
            val idx = baseFactor * machineId + i
            val age = Random.nextInt(90) + 10
            bspToAgent(new PersonAgent(idx, 
                age, 
                (0 until totalMachines * baseFactor).filter(j => (Random.nextDouble() < p) && (j != idx)), 
                Random.nextBoolean(), 
                if (Random.nextInt(100)==0) 0 else 2,
                if (age > 60) 1 else 0))
        })
    }
}

object SBMUnoptScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        assert(machineId < totalMachines)
        val p: Double = 0.01
        val q: Double = 0
        val startingIndex = machineId * baseFactor
        val graph = GraphFactory.stochasticBlock(baseFactor, p, q, 5, startingIndex)

        (0 until baseFactor).map(i => {
            val idx = startingIndex + i
            val age = Random.nextInt(90) + 10
            bspToAgent(new PersonAgent(idx, 
                age, 
                graph.adjacencyList()(idx),
                Random.nextBoolean(), 
                if (Random.nextInt(100)==0) 0 else 2,
                if (age > 60) 1 else 0))
        })
    }
}