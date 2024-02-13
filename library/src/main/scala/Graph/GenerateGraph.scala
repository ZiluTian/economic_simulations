package cloudcity.lib
package Graph

import scala.util.Random
import scala.math.floor

trait Graph {
    val g: Map[Long, Iterable[Long]]
}

object Graph{
    implicit def toGraph(g: Graph): Map[Long, Iterable[Long]] = g.g
}

case class ErdosRenyiGraph(totalVertices: Int, edgeProb: Double, startingIndex: Int = 0) extends Graph {
    val g: Map[Long, Iterable[Long]] = {
        val nodes = Range(startingIndex, totalVertices+startingIndex)
        Range(0, totalVertices).map(i => {
            (i.toLong + startingIndex, nodes.filter(n => {
                (n!=i) && edgeProb > Random.nextDouble() 
            }).map(_.toLong))
        }).toMap
    }
}

case class SBMGraph(totalVertices: Int, p: Double, q: Double, blocks: Int, startingIndex: Int = 0) extends Graph {
    val g: Map[Long, Iterable[Long]] = {
        val verticesPerBlock: Int = floor(totalVertices / blocks).toInt
        var graph: Map[Long, Iterable[Long]] = Map[Long, Iterable[Long]]()
        // The edge probability between two vertices in the same block is p
        Range(0, blocks).foreach(i => {
            val offset = startingIndex + verticesPerBlock * i
            if (i == (blocks-1)) {
                graph = graph ++ ErdosRenyiGraph(totalVertices + startingIndex - offset, p, offset)
            } else {
                graph = graph ++ ErdosRenyiGraph(verticesPerBlock, p, offset)
            }
        })
        // The edge probability between two vertices in different blocks is q
        if (q > 0) {
            Range(0, totalVertices).foreach(i => {
                val currentBlock: Int = floor(i / verticesPerBlock).toInt
                val verticesInOtherBlocks = if (currentBlock > 0){
                    Range(0, totalVertices).slice(0, currentBlock*verticesPerBlock) ++ Range(verticesPerBlock*(i+1), totalVertices)
                } else {
                    Range(verticesPerBlock, totalVertices)
                }
                graph = graph + (i.toLong -> (graph(i) ++ verticesInOtherBlocks.filter(i => (q > Random.nextDouble())).map(_.toLong)))
            })
        }
        graph
    }
}

case class Torus2DGraph(width: Int, height: Int, startingIndex: Int = 0) extends Graph {
    val g: Map[Long, IndexedSeq[Long]] = {
        Range(0, width * height).map(index => {
            val x = index % width
            val y = index / width

            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + height) % height
            } yield dy * width + dx
            (index.toLong + startingIndex, neighbors.map(n => n.toLong + startingIndex))
        }).toMap
    }
}

case class BipartiteGraph(set1Size: Int, set2Size: Int, startingIndex: Int = 0) extends Graph {
    val g: Map[Long, Iterable[Long]] = {        
        (Range(startingIndex, set1Size + startingIndex).map(i => {
            (i.toLong, Range(set1Size + startingIndex, set2Size).map(_.toLong))
        }) ++ Range(startingIndex + set1Size, startingIndex + set1Size + set2Size).map(i => {
            (i.toLong, Range(startingIndex, startingIndex +startingIndex + set1Size).map(_.toLong))
        })).toMap
    }
}