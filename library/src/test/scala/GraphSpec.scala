package cloudcity.lib
package Graph
package test

import org.scalatest.FlatSpec
import GraphFactory._

class GraphSpec extends FlatSpec {

  "Each cell in a 2D grid" should "have eight neighbors" in {
    val graph1 = torus2D(3, 3)
    graph1.adjacencyList().foreach(i => assert(i._2.size == 8))
    assert(graph1.adjacencyList().size == 9)

    val graph2 = torus2D(100, 100)
    graph2.adjacencyList().foreach(i => assert(i._2.size == 8))
    assert(graph2.adjacencyList().size == 10000)
  }

  "Each cell in a 2D grid with offsets" should "have eight neighbors" in {
    val graph1 = torus2D(3, 3, 13)
    graph1.adjacencyList().foreach(i => assert(i._2.size == 8))
    assert(graph1.adjacencyList().size == 9)

    val graph2 = torus2D(100, 100, 11)
    graph2.adjacencyList().foreach(i => assert(i._2.size == 8))
    assert(graph2.adjacencyList().size == 10000)
  }

  "SBM graph" should "generate a line for each node" in {
    val totalNodes = 36
    val graph = stochasticBlock(totalNodes, 0.1, 0, 5)
    assert(graph.adjacencyList().size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = stochasticBlock(totalNodes2, 0.1, 0, 5)
    assert(graph2.adjacencyList().size == totalNodes2)
  }

  "SBM graph with offsets" should "generate a line for each node" in {
    val totalNodes = 36
    val graph = stochasticBlock(totalNodes, 0.1, 0, 5, 10)
    assert(graph.adjacencyList().size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = stochasticBlock(totalNodes2, 0.1, 0, 5, 1)
    assert(graph2.adjacencyList().size == totalNodes2)
  }


  "ERM graph" should "generate a line for each node" in {
    val totalNodes = 71
    val graph = erdosRenyi(totalNodes, 0.1)
    assert(graph.adjacencyList().size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = erdosRenyi(totalNodes2, 0.1)
    assert(graph2.adjacencyList().size == totalNodes2)
  }

  "ERM graph with offsets" should "generate a line for each node" in {
    val totalNodes = 71
    val graph = erdosRenyi(totalNodes, 0.1, 1)
    assert(graph.adjacencyList().size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = erdosRenyi(totalNodes2, 0.1, 11)
    assert(graph2.adjacencyList().size == totalNodes2)
  }
}