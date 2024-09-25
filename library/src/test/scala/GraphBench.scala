package cloudcity.lib
package Graph
package test

import org.scalatest.FlatSpec
import GraphFactory._
import scala.util.Random
import scala.math.floor

class GenSBMGraphBench extends FlatSpec {
  val startingIndex: Long = 0
  val intraProb: Double = 0.01
  val interProb: Double = 0.001
  val blocks: Int = 5
  val offset = 0

  List(1000, 10000).foreach(totalVertices => {
    val verticesPerBlock: Long = floor(totalVertices / blocks).toLong
    // f"Constructing SBM graphs sequentially for $totalVertices vertices" should "run" in {
    //   val t0 = System.currentTimeMillis()
    //   var graph = Map.empty[Long, Iterable[Long]]
    //     (0L until blocks).foreach(i => {
    //         val offset = startingIndex + verticesPerBlock * i
    //         if (i == (blocks-1)) {
    //             graph = graph ++ (new ErdosRenyiGraph(totalVertices + startingIndex - offset, intraProb, offset)).adjacencyList
    //         } else {
    //             graph = graph ++ (new ErdosRenyiGraph(verticesPerBlock, intraProb, offset)).adjacencyList
    //         }
    //     })
    //     // The edge probability between two vertices in different blocks is q
    //     if (interProb > 0) {
    //       val intraGraph = (0L until blocks).par.flatMap(i => {
    //           (verticesPerBlock*i until math.min(verticesPerBlock*(i+1), totalVertices)).map(j => {
    //               (j, (j+1 until totalVertices).filter(_ => Random.nextDouble() < interProb).to[collection.mutable.Set])
    //           }).toMap.seq
    //       }).toMap.seq

    //       intraGraph.foreach {case (k, v) => {
    //           v.foreach(n => {
    //               intraGraph(n) += k
    //           })
    //       }}

    //       intraGraph.foreach {case (k, v) => {
    //           graph = graph + (k -> (graph(k) ++ v))
    //       }}
    //     }
    //     //     (0L until totalVertices).foreach(i => {
    //     //         val currentBlock: Int = floor(i / verticesPerBlock).toInt
    //     //         val verticesInOtherBlocks = if (currentBlock > 0){
    //     //             (0L until totalVertices).slice(0, currentBlock*verticesPerBlock.toInt) ++ (verticesPerBlock*(i+1) until totalVertices)
    //     //         } else {
    //     //             (verticesPerBlock until totalVertices)
    //     //         }
    //     //         graph = graph + (i -> (graph(i) ++ verticesInOtherBlocks.filter(_ => interProb > Random.nextDouble())))
    //     //     })
    //     // }

    //   val t1 = System.currentTimeMillis()
    //   println(f"Constructing SBM sequentially uses ${t1-t0} ms for ${totalVertices} vertices")
    // }

    f"Constructing SBM graphs in parallel for $totalVertices" should "run" in {
      val t0 = System.currentTimeMillis()
      var graph: Map[Long, Iterable[Long]] = (0L until blocks).par.map(i => {
          val offset = startingIndex + verticesPerBlock * i
          if (i == (blocks-1)) {
              new ErdosRenyiGraph(totalVertices + startingIndex - offset, intraProb, offset).adjacencyList
          } else {
              new ErdosRenyiGraph(verticesPerBlock, intraProb, offset).adjacencyList
          }
      }).reduce(_ ++ _).seq

      // The edge probability between two vertices in different blocks is q
      if (interProb > 0) {
            var intraGraph = (0L until blocks).par.flatMap(i => {
                (verticesPerBlock*i until math.min(verticesPerBlock*(i+1), totalVertices)).map(j => {
                    (j, (j+1 until totalVertices).filter(_ => Random.nextDouble() < interProb).toSet)
                }).toMap.seq
            }).toMap.seq

            intraGraph.foreach {case (k, v) => {
                v.foreach(n => {
                    intraGraph = intraGraph + (n -> (intraGraph.getOrElse(n, Set()) + k))
                })
            }}

            intraGraph.foreach {case (k, v) => {
                graph = graph + (k -> (graph(k) ++ v))
            }}
        }
      // if (interProb > 0) {
      //     val intraGraph = (0L until blocks).par.flatMap(i => {
      //         (verticesPerBlock*i until math.min(verticesPerBlock*(i+1), totalVertices)).map(j => {
      //             (j, (j+1 until totalVertices).filter(_ => Random.nextDouble() < interProb).to[collection.mutable.Set])
      //         }).seq
      //     }).toMap.seq

      //     intraGraph.foreach {case (k, v) => {
      //         v.foreach(n => {
      //             intraGraph(n) += k
      //         })
      //     }}

      //     intraGraph.foreach {case (k, v) => {
      //         graph = graph + (k -> (graph(k) ++ v))
      //     }}
      // }

      val t1 = System.currentTimeMillis()
      println(f"Constructing SBM in parallel uses ${t1-t0} ms for ${totalVertices} vertices")
    }
  })
}

class GenERMGraphBench extends FlatSpec {
  val startingIndex: Int = 0
  val edgeProb: Double = 0.01

// Using par is much slower than sequential implementation for common cases, despite higher CPU util
// Constructing using sequential construct uses 52 ms for 1000 vertices
// Constructing using sequential construct (set-based) uses 45 ms for 1000 vertices
// Constructing using par (v1) uses 366 ms for 1000 vertices
// Constructing using par (v2) uses 203 ms for 1000 vertices
// Constructing using sequential construct uses 1598 ms for 10000 vertices
// Constructing using sequential construct (set-based) uses 1613 ms for 10000 vertices
// Constructing using par (v1) uses 16375 ms for 10000 vertices
// Constructing using par (v2) uses 20665 ms for 10000 vertices
// Constructing using sequential construct uses 41521 ms for 50000 vertices
// Constructing using sequential construct (set-based) uses 40804 ms for 50000 vertices

  List(1000, 10000).foreach(totalVertices => {
    f"Constructing an ERM graph sequentially for $totalVertices vertices" should "be slower" in {
        val t0 = System.currentTimeMillis()
        var graph = Map.empty[Int, List[Int]]
        val rand = new Random()
        (startingIndex until (startingIndex + totalVertices)).map { i =>
            val neighbors = (i+1 until (startingIndex + totalVertices)).filter(_ => rand.nextDouble() < edgeProb)
            graph = graph + (i -> (graph.getOrElse(i, List()) ++ neighbors))
            neighbors.foreach(n => {
                graph = graph + (n -> (i :: graph.getOrElse(n, List())))
            })
        }
        val t1 = System.currentTimeMillis()
        println(f"Constructing using sequential construct uses ${t1-t0} ms for ${totalVertices} vertices")
    }

    f"Constructing an ERM graph sequentially (set-based) for $totalVertices vertices" should "be slower" in {
        val t0 = System.currentTimeMillis()
        var graph = Map.empty[Int, Set[Int]]
        val rand = new Random()
        (startingIndex until (startingIndex + totalVertices)).map { i =>
            val neighbors = (i+1 until (startingIndex + totalVertices)).filter(_ => rand.nextDouble() < edgeProb)
            graph = graph + (i -> (graph.getOrElse(i, Set()) ++ neighbors))
            neighbors.foreach(n => {
                graph = graph + (n -> (graph.getOrElse(n, Set()) + i))
            })
        }
        val t1 = System.currentTimeMillis()
        println(f"Constructing using sequential construct (set-based) uses ${t1-t0} ms for ${totalVertices} vertices")
    }

    f"Constructing an ERM graph in parallel (v1) for $totalVertices vertices" should "be fast" in {
        val t0 = System.currentTimeMillis()
        val rand = new Random()
        val graph = (startingIndex until (startingIndex + totalVertices)).par.map(i => {
            (i, (i+1 until (startingIndex + totalVertices)).par.filter(_ => (rand.nextDouble() < edgeProb)).to[collection.mutable.Set])
        }).seq.toMap
        graph.foreach(i => {
            i._2.foreach(k => {
                graph(k) += i._1
            })
        })
        val t1 = System.currentTimeMillis()
        println(f"Constructing using par (v1) uses ${t1-t0} ms for ${totalVertices} vertices")  
    }

    f"Constructing an ERM graph in parallel (v2) for $totalVertices vertices" should "be fast" in {
        val t0 = System.currentTimeMillis()
        val rand = new Random()
        val graph = (startingIndex until (startingIndex + totalVertices)).par.map(i => {
          // no more par in filter
            (i, (i+1 until (startingIndex + totalVertices)).filter(_ => (rand.nextDouble() < edgeProb)).to[collection.mutable.Set])
        }).seq.toMap
        graph.foreach(i => {
            i._2.foreach(k => {
                graph(k) += i._1
            })
        })
        val t1 = System.currentTimeMillis()
        println(f"Constructing using par (v2) uses ${t1-t0} ms for ${totalVertices} vertices")  
    }
  })
}