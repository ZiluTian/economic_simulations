package cloudcity.lib
package Graph

import scala.util.Random
import scala.math.floor

trait Graph {
    def adjacencyList(): Map[Long, Iterable[Long]]
    def nodes: Iterable[Long]
    def edges: Iterable[(Long, Long)]
}

object GraphFactory{
    def erdosRenyi(totalVertices: Long, edgeProb: Double, startingIndex: Long=0L): Graph = new ErdosRenyiGraph(totalVertices, edgeProb, startingIndex)
    def stochasticBlock(totalVertices: Long, intraProb: Double, interProb: Double, blocks: Int, startingIndex: Long=0L): Graph = new SBMGraph(totalVertices, intraProb, interProb, blocks, startingIndex)
    def torus2D(width: Int, height: Int, startingIndex: Long=0L) = new Torus2DGraph(width, height, startingIndex)
    def bipartite(set1Size: Long, set2Size: Long, startingIndex: Long = 0L) = new BipartiteGraph(set1Size, set2Size, startingIndex)
}

class ErdosRenyiGraph(totalVertices: Long, edgeProb: Double, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(totalVertices, edgeProb, startingIndex)
    
    private def generateGraph(totalVertices: Long, edgeProb: Double, startingIndex: Long): Map[Long, Iterable[Long]] = {
        val rand = new Random()
        (startingIndex until (startingIndex + totalVertices)).map { i =>
            val neighbors = (startingIndex until (startingIndex + totalVertices)).filter(j => i != j && rand.nextDouble() < edgeProb)
            i -> neighbors
        }.toMap
    }
    
    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}

class SBMGraph(totalVertices: Long, intraProb: Double, interProb: Double, blocks: Int, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(totalVertices, intraProb, interProb, blocks, startingIndex)
    
    private def generateGraph(totalVertices: Long, intraProb: Double, interProb: Double, blocks: Int, startingIndex: Long): Map[Long, Iterable[Long]] = {
        val verticesPerBlock: Long = floor(totalVertices / blocks).toLong
        var graph = Map.empty[Long, Iterable[Long]]
        (0L until blocks).foreach(i => {
            val offset = startingIndex + verticesPerBlock * i
            if (i == (blocks-1)) {
                graph = graph ++ (new ErdosRenyiGraph(totalVertices + startingIndex - offset, interProb, offset)).adjacencyList
            } else {
                graph = graph ++ (new ErdosRenyiGraph(verticesPerBlock, interProb, offset)).adjacencyList
            }
        })
        // The edge probability between two vertices in different blocks is q
        if (interProb > 0) {
            (0L until totalVertices).foreach(i => {
                val currentBlock: Int = floor(i / verticesPerBlock).toInt
                val verticesInOtherBlocks = if (currentBlock > 0){
                    (0L until totalVertices).slice(0, currentBlock*verticesPerBlock.toInt) ++ (verticesPerBlock*(i+1) until totalVertices)
                } else {
                    (verticesPerBlock until totalVertices)
                }
                graph = graph + (i -> (graph(i) ++ verticesInOtherBlocks.filter(_ => interProb > Random.nextDouble())))
            })
        }
        graph
    }
    
    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}

class Torus2DGraph(width: Int, height: Int, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(width, height, startingIndex)
    
    private def generateGraph(width: Int, height: Int, startingIndex: Long): Map[Long, Iterable[Long]] = {
        (0L until width * height).map(index => {
            val x: Long = index % width
            val y: Long = index / width
            val neighbors = for {
                i <- -1 to 1
                j <- -1 to 1
                if !(i == 0 && j == 0)
                    dx = (x + i + width) % width
                    dy = (y + j + height) % height
            } yield dy * width + dx
            (index + startingIndex, neighbors.map(n => n + startingIndex))
        }).toMap
    }

    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}

class BipartiteGraph(set1Size: Long, set2Size: Long, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(set1Size, set2Size, startingIndex)
    
    private def generateGraph(set1Size: Long, set2Size: Long, startingIndex: Long): Map[Long, Iterable[Long]] = {
        var graph = Map.empty[Long, Iterable[Long]]
        val set1 = startingIndex until (set1Size + startingIndex)
        val set2 = (set1Size + startingIndex) until (set1Size + startingIndex + set2Size)
        // Create edges between every node in set1 and every node in set2
        set1.foreach(n => graph = graph + (n -> set2))
        set2.foreach(n => graph = graph + (n -> set1))
        graph
    }

    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}