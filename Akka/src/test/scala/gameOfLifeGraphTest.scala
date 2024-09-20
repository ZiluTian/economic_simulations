package simulation.akka
package test

import simulation.akka.API._
import org.scalatest.FlatSpec
import meta.API.{SimulationData, DeforestationStrategy}
import cloudcity.lib.Graph._
import BSPModel._
import scala.util.Random
import meta.runtime.{BoolVectorMessage, IntArrayMessage, Actor}
import BSPModel.example.gameOfLife._

class gameOfLifeGraphTest extends scaleUpTest {
    // Look for adjacent partitions
    class horizontalPartition(partId: Int, totalPartitions: Int, width: Int, height: Int) extends Actor {
        id = partId.toLong
        var readOnly = Array.tabulate(height+2, width)((x, y) => Random.nextBoolean())
        var readWrite = readOnly.map(i => i.clone)

        override def run(): Int = {
            receivedMessages.foreach(i => {
                if (i.asInstanceOf[BoolVectorMessage].value(0)) {
                    readOnly(0) = i.asInstanceOf[BoolVectorMessage].value.tail.toArray
                } else {
                    readOnly(height+1) = i.asInstanceOf[BoolVectorMessage].value.tail.toArray
                }
            })

            (1 until height).foreach(row => {
                (0 until width).foreach(col => {
                    var totalAlive: Int = 0
                    for {
                        i <- -1 to 1
                        j <- -1 to 1
                        if !(i == 0 && j == 0)
                            dx = col + i
                            dy = (row + j + width) % width
                    } {
                        if (readOnly(dx)(dy)) {
                            totalAlive += 1
                        }
                    }

                    if (totalAlive == 3) {
                        readWrite(row)(col) = true
                    } else if (totalAlive < 3 || totalAlive > 3) {
                        readWrite(row)(col) = false
                    } else {
                        readWrite(row)(col) = readOnly(row)(col)
                    }
                })
            })
            readOnly = readWrite.map(_.clone)

            // send to adjacent partitions, use T/F to encode directions
            sendMessage((partId+1)%totalPartitions, BoolVectorMessage(true +: readOnly(height).toVector))
            sendMessage((partId-1+totalPartitions)%totalPartitions, BoolVectorMessage(false +: readOnly(1).toVector))
            1
        }
    }

    def gen(numPartition: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val height: Int = (baseFactor/width).toInt
        (0 until numPartition).map(i => {
            new horizontalPartition(i, numPartition, width, height)
        })
    }
}