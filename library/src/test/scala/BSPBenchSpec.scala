package BSPModel
package test

import cloudcity.lib.Graph
import scala.util.Random

class BSPBenchUtilSpec extends BSPModel.test.BSPBenchSuite {

    val totalRounds = 200
    val experimentName = "partition"

    test("Partition 2D torus graph") {
        val width = 100
        val height = 100
        val graph = Graph.GraphFactory.torus2D(width, height)

        (1 to 100).foreach(i => {
            val separatedParts: Iterable[BSPModel.Graph[BSPId]] = partition(graph, i)
            assert(separatedParts.map(_.vertices.size).sum == width*height)
            println(f"Passed for i $i")
        })
    }

    test("Partition ERM graph") {
        val totalVertices = 10000
        val interProb = 0.01
        val graph = Graph.GraphFactory.erdosRenyi(totalVertices, interProb)

        (1 to 100).foreach(i => {
            val separatedParts: Vector[BSPModel.Graph[BSPId]] = partition(graph, i).toVector
            assert(separatedParts.map(_.vertices.size).sum == totalVertices)
            separatedParts(Random.nextInt(i)).inEdges.foreach(i => {
                assert(i._2.toSet.subsetOf(separatedParts(i._1).vertices))
            })
            separatedParts(Random.nextInt(i)).outEdges.foreach(i => {
                assert(i._2.toSet.subsetOf(separatedParts(i._1).vertices))
            })
            println(f"Passed for i $i")
        })
    }

    test("Partition SBM graph") {
        val totalVertices = 10000
        val interProb = 0.01
        val graph = Graph.GraphFactory.stochasticBlock(totalVertices, interProb, 0.01, 5)

        (1 to 100).foreach(i => {
            val separatedParts: Iterable[BSPModel.Graph[BSPId]] = partition(graph, i)
            assert(separatedParts.map(_.vertices.size).sum == totalVertices)
            separatedParts(Random.nextInt(i)).inEdges.foreach(i => {
                assert(i._2.toSet.subsetOf(separatedParts(i._1).vertices))
            })
            separatedParts(Random.nextInt(i)).outEdges.foreach(i => {
                assert(i._2.toSet.subsetOf(separatedParts(i._1).vertices))
            })
            println(f"Passed for i $i")
        })
    }
}