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

object gameOfLifeFusionWithRemoteMessageScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val width: Int = 100
        val totalHeight: Int = (baseFactor * totalMachines / width).toInt
        val height: Int = (baseFactor / width / localScaleFactor).toInt
        val startingIndex = machineId * baseFactor

        val cells = (startingIndex until startingIndex + baseFactor).map(index => {
            val x: Int = index % width
            val y: Int = index / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + totalHeight) % totalHeight
            } yield dy * width + dx
            (index, new Cell(index, neighbors.toSeq))
        }).toMap

        (0 until localScaleFactor).map(i => {
            val bspGraph = partition2DArray(machineId * localScaleFactor + i, localScaleFactor * totalMachines, width, height)
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = i + machineId * localScaleFactor
                val topo = bspGraph
                val members = bspGraph.vertices.map(j => bspToAgent(cells(j))).toList
            }
            partToAgent.fuseWithRemoteMsgIntVectorAgent(part)
        }).toVector
    }
}

object stockMarketAggregateFusionWithRemoteMessageScaleOutTest extends scaleOutTest with App {
    import BSPModel.example.stockMarket.ConditionActionRule._

    override def main(args: Array[String]): Unit = {
        exec(args, 200)
    }

    class MarketAgent(pos: BSPId, neighbors: Seq[BSPId], initialStockPrice: Double) extends BSP with ComputeMethod {
        type State = Market 
        type InMessage = (Int, Int)
        type SerializeFormat = Vector[Double]

        var state: Market = {
            val stock = new Stock(0.01)
            stock.priceAdjustmentFactor = 0.1 / neighbors.size
            // initial st
            Market(stock, stock.updateMarketInfo(initialStockPrice, stock.getDividend), initialStockPrice, 0, 0, 0)
        }
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 

        // Transform from Vector[Double] to (Int, Int)
        override def deserialize(in: SerializeFormat): InMessage = {
            if (in.size == 1) {
                val m = in.head.toInt
                if (m == buy){
                    (1, 0)
                } else if (m == sell) {
                    (0, 1)
                } else {
                    (0, 0)
                }   
            } else if (in.size ==2) {
                (in(0).toInt, in(1).toInt)
            } else {
                throw new Exception("Shouldn't happen!")
            }
        }

        def partialCompute(ms: Iterable[(Int, Int)]): Option[(Int, Int)] = {
            // println(f"$id partial compute is called with $ms")
            ms match {
                case Nil => None
                case _ => 
                    Some(ms.foldLeft((0, 0)){
                        case ((x, y), (e1, e2)) =>
                            (x+e1, y+e2)              // Add to odd list
                    })
            }
        }

        def updateState(s: Market, m: Option[(Int, Int)]): Market = {
            m match {
                case None => s
                case Some(x) => {
                    s.buyOrders += x._1
                    s.sellOrders += x._2
                }
            }
            s.stockPrice = s.stock.priceAdjustment(state.buyOrders, state.sellOrders)
            // println(f"Stock price is ${s.stockPrice} buy ${state.buyOrders} sell ${state.sellOrders}")
            s.dividendPerShare = s.stock.getDividend()
            s.marketState = s.stock.updateMarketInfo(s.stockPrice, s.dividendPerShare)
            state.buyOrders = 0
            state.sellOrders = 0
            s
        }

        def stateToMessage(s: Market):SerializeFormat = {
            Vector[Double](s.stockPrice, s.dividendPerShare, s.marketState(0), s.marketState(1), s.marketState(2))
        }
    } 

    // aggregates values from traders in the neighbors    
    class ActionAggregator(pos: BSPId, neighbors: Seq[BSPId]) extends BSP with ComputeMethod {
        type State = (Int, Int)
        type InMessage = (Int, Int)
        type SerializeFormat = Vector[Double]

        var state = (0, 0)
        override val id = pos
        val receiveFrom = FixedCommunication(neighbors) 

        // receive 
        // Transform from Vector[Double] to (Int, Int)
        override def deserialize(in: SerializeFormat): InMessage = {
            val m = in.head.toInt
            if (m == buy){
                (1, 0)
            } else if (m == sell) {
                (0, 1)
            } else {
                (0, 0)
            }
        }

        def partialCompute(ms: Iterable[(Int, Int)]): Option[(Int, Int)] = {
            // println(f"$id partial compute is called with $ms")
            ms match {
                case Nil => None
                case _ => 
                    Some(ms.foldLeft((0, 0)){
                        case ((x, y), (e1, e2)) =>
                            (x+e1, y+e2)              // Add to odd list
                    })
            }
        }

        def updateState(s: State, m: Option[(Int, Int)]): State = {
            m match {
                case None => s
                case Some(x) => {
                    (s._1 + x._1, s._2 + x._2)
                }
            }
        }

        def stateToMessage(s: State):SerializeFormat = {
            Vector[Double](s._1, s._2)
        }
    }


    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val initialStockPrice: Double = 100
        val budget: Double = 1000
        val interestRate = 0.0001
        var adjList = Map[Int, List[Int]]()
        val offset = machineId * baseFactor        
        val elementsPerPartition = baseFactor / localScaleFactor
        val tradersPerPartition = elementsPerPartition - 1
        // Connect the market/proxy with local traders
        (0 until localScaleFactor).foreach(i => {
            adjList = adjList + ((offset + i * elementsPerPartition) -> (0 :: ((offset + i * elementsPerPartition + 1) until (offset + (i+1) * elementsPerPartition)).toList))
            ((offset + i * elementsPerPartition + 1) until (offset + (i+1) * elementsPerPartition)).foreach(j => {
                adjList = adjList + (j -> List(0))
            })
        })

        // 0 should be connected with proxy markets (0, List(10, 20, 30))
        if (machineId == 0) {
            adjList = adjList + (0 -> (adjList(0).filter(_!=0) ++ (1 until totalMachines * localScaleFactor).map(i => i * elementsPerPartition)).toList)
        }

        val cells: Map[Int, Actor] = 
            adjList.map(i => {
                if (i._1 == 0) {
                    (i._1, bspToAgent(new MarketAgent(i._1, i._2, initialStockPrice)))
                } else if (i._1 % elementsPerPartition ==0) {
                    (i._1, bspToAgent(new ActionAggregator(i._1, i._2)))
                } else {
                    (i._1, bspToAgent(new TraderAgent(i._1, i._2, budget, interestRate)))
                }
            })

        (0 until localScaleFactor).par.map(i => {
            val globalId = machineId * localScaleFactor + i
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = globalId
                val topo = new BSPModel.Graph[BSPId] {
                    val vertices = (globalId*elementsPerPartition until (globalId+1)*elementsPerPartition).toSet
                    val edges = Map()
                    val inExtVertices = if (globalId == 0) {
                        // Receive aggregated action from other partitions
                        (1 until totalMachines * localScaleFactor).map(j => {
                            (j, Vector(j*elementsPerPartition))
                        }).toMap    
                    } else {
                        // Receive market state from 0
                        Map(0 -> Vector(0))
                    }
                    val outIntVertices = if (globalId == 0) {
                        // Send the market value to other partitions
                        (1 until totalMachines * localScaleFactor).map(j => {
                            (j, Vector(0))
                        }).toMap
                    } else {
                        // Send aggregated action to partition 0
                        Map(0 -> Vector(globalId*elementsPerPartition))
                    }
                }
                // println(f"Partition ${i} has vertices ${topo.vertices} ${topo.inExtVertices}")
                val members = (globalId*elementsPerPartition until (globalId+1)*elementsPerPartition).map(j => cells(j)).toList
            }
            partToAgent.fuseWithRemoteMsgDoubleVectorVectorAgent(part)
        }).seq.toVector
    } 
}

object ERMFusionWithRemoteMessageScaleOutTest extends scaleOutTest with App {

    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    lazy val cuts: (Int, Int) => Map[BSPId, Set[BSPId]] = (partitionSize: Int, totalPartitions: Int) => {
        val p: Double = 0.01
        val rand = new Random(100)
        var graph = Map.empty[BSPId, Set[BSPId]]
        (0 until totalPartitions).foreach ({ i => 
            (0 until partitionSize).foreach(v => {
                val vid = i * partitionSize + v
                val neighbors = ((i + 1) * partitionSize until totalPartitions * partitionSize).filter(_ => rand.nextDouble() < p)
                graph = graph + ((i * partitionSize + v) -> (graph.getOrElse(vid, Set()) ++ neighbors))
                neighbors.foreach(n => {
                    graph = graph + (n -> (graph.getOrElse(n, Set()) + vid))
                })
            })
        })
        graph
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p: Double = 0.01
        val ermGraph = GraphFactory.erdosRenyi(baseFactor * totalMachines, p)
        println(f"ERM graph at $machineId has been constructed!")
        val partitionedERMGraph = partition(ermGraph, totalMachines * localScaleFactor)
        println(f"Partitioned ERM graph at $machineId has been constructed!")
        val partitionedGraphs = partitionedERMGraph.slice(machineId * localScaleFactor, (machineId + 1) * localScaleFactor).par
        // val cells = genPopulation(partitionedGraphs.flatMap(i => i.vertices).map(i => (i, ermGraph.adjacencyList().getOrElse(i, List()))).seq)

        partitionedGraphs.zipWithIndex.par.map(i => {
            val partId = localScaleFactor * machineId + i._2
            // println(f"Partition ${partId} incoming external vertices are ${i._1.inExtVertices}")
            // println(f"Partition ${partId} outgoing internal vertices are ${i._1.outIntVertices}")
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = partId
                val topo = i._1
                val members = 
                    i._1.vertices.map(j => {
                        val age = Random.nextInt(90) + 10
                        bspToAgent(new PersonAgent(j, 
                            age, 
                            ermGraph.adjacencyList().getOrElse(j, List()),
                            Random.nextBoolean(), 
                            if (Random.nextInt(100)==0) 0 else 2,
                            if (age > 60) 1 else 0))
                }).toList
            }
            println(f"Local partition ${partId} at $machineId has been constructed!")
            partToAgent.fuseWithRemoteMsgDoubleVectorAgent(part)
        }).seq.toVector
    }
}

object SBMFusionWithRemoteMessageScaleOutTest extends scaleOutTest with App {
    override def main(args: Array[String]): Unit = {
        exec(args, 50)
    }

    def gen(machineId: Int, totalMachines: Int): IndexedSeq[Actor] = {
        val p =0.01
        val q = 0
        val numBlocks = 5
        val startingIndex = machineId * baseFactor
        val partitionSize = baseFactor / localScaleFactor
        val graph = GraphFactory.stochasticBlock(baseFactor, p, q, 5, startingIndex)
       
        partition(graph, localScaleFactor, machineId * localScaleFactor).zipWithIndex.par.map(i => {
            val partId = i._2 + machineId * localScaleFactor
            val part = new BSPModel.Partition {
                type Member = Actor
                type NodeId = BSPId
                type Value = BSP
                val id = partId
                val topo = i._1
                val members = i._1.vertices.map(j => {
                    val age = Random.nextInt(90) + 10
                    bspToAgent(new PersonAgent(j, 
                        age, 
                        graph.adjacencyList().getOrElse(j, List()),
                        Random.nextBoolean(), 
                        if (Random.nextInt(100)==0) 0 else 2,
                        if (age > 60) 1 else 0))
                }).toList
            }
            println(f"Local partition ${partId} at $machineId has been constructed!")
            partToAgent.fuseWithRemoteMsgDoubleVectorAgent(part)
        }).seq.toVector
    }
}