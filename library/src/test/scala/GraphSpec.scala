package cloudcity.lib
package Graph
package test

import org.scalatest.FlatSpec

class GraphSpec extends FlatSpec {

  "Each cell in a 2D grid" should "have eight neighbors" in {
    val graph1 = Torus2DGraph(3, 3)
    graph1.foreach(i => assert(i._2.size == 8))
    assert(graph1.size == 9)

    val graph2 = Torus2DGraph(100, 100)
    graph2.foreach(i => assert(i._2.size == 8))
    assert(graph2.size == 10000)
  }

  "Each cell in a 2D grid with offsets" should "have eight neighbors" in {
    val graph1 = Torus2DGraph(3, 3, 13)
    graph1.foreach(i => assert(i._2.size == 8))
    assert(graph1.size == 9)

    val graph2 = Torus2DGraph(100, 100, 11)
    graph2.foreach(i => assert(i._2.size == 8))
    assert(graph2.size == 10000)
  }

  "SBM graph" should "generate a line for each node" in {
    val totalNodes = 36
    val graph = SBMGraph(totalNodes, 0.1, 0, 5)
    assert(graph.size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = SBMGraph(totalNodes2, 0.1, 0, 5)
    assert(graph2.size == totalNodes2)
  }

  "SBM graph with offsets" should "generate a line for each node" in {
    val totalNodes = 36
    val graph = SBMGraph(totalNodes, 0.1, 0, 5, 10)
    assert(graph.size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = SBMGraph(totalNodes2, 0.1, 0, 5, 1)
    assert(graph2.size == totalNodes2)
  }


  "ERM graph" should "generate a line for each node" in {
    val totalNodes = 71
    val graph = ErdosRenyiGraph(totalNodes, 0.1)
    assert(graph.size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = ErdosRenyiGraph(totalNodes2, 0.1)
    assert(graph2.size == totalNodes2)
  }

  "ERM graph with offsets" should "generate a line for each node" in {
    val totalNodes = 71
    val graph = ErdosRenyiGraph(totalNodes, 0.1, 1)
    assert(graph.size == totalNodes)
    val totalNodes2 = 1000
    val graph2 = ErdosRenyiGraph(totalNodes2, 0.1, 11)
    assert(graph2.size == totalNodes2)
  }
}