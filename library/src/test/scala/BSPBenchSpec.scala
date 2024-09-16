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
            val separatedParts: IndexedSeq[BSPModel.Graph[BSPId]] = partition(graph, i)
            assert(separatedParts.map(_.vertices.size).sum == width*height)
            (0 until separatedParts.size).foreach(p => {
                separatedParts(p).outIntVertices.foreach(j => {
                    assert(separatedParts(j._1).inExtVertices(p) == j._2)
                })
            })
            println(f"Passed for i $i")
        })
    }

    test("Partition ERM graph") {
        val totalVertices = 10000
        val interProb = 0.01
        val graph = Graph.GraphFactory.erdosRenyi(totalVertices, interProb)

        (1 to 100).foreach(i => {
            val separatedParts: IndexedSeq[BSPModel.Graph[BSPId]] = partition(graph, i)
            assert(separatedParts.map(_.vertices.size).sum == totalVertices)
            (0 until separatedParts.size).foreach(p => {
                separatedParts(p).outIntVertices.foreach(j => {
                    assert(separatedParts(j._1).inExtVertices(p) == j._2)
                })
            })
            println(f"Passed for i $i")
        })
    }

    test("Partition SBM graph") {
        val totalVertices = 10000
        val interProb = 0.01
        val graph = Graph.GraphFactory.stochasticBlock(totalVertices, interProb, 0.01, 5)

        (1 to 100).foreach(i => {
            val separatedParts: IndexedSeq[BSPModel.Graph[BSPId]] = partition(graph, i)
            assert(separatedParts.map(_.vertices.size).sum == totalVertices)
            (0 until separatedParts.size).foreach(p => {
                separatedParts(p).outIntVertices.foreach(j => {
                    assert(separatedParts(j._1).inExtVertices(p) == j._2)
                })
            })
            println(f"Passed for i $i")
        })
    }
}