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
        var graph = Map.empty[Long, Set[Long]]
        val rand = new Random()
        (startingIndex until (startingIndex + totalVertices)).map { i =>
            val neighbors = (i+1 until (startingIndex + totalVertices)).filter(_ => rand.nextDouble() < edgeProb)
            graph = graph + (i -> (graph.getOrElse(i, Set()) ++ neighbors))
            neighbors.foreach(n => {
                graph = graph + (n -> (graph.getOrElse(n, Set()) + i))
            })
        }
        graph
    }
    
    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.toIterable.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}

class SBMGraph(totalVertices: Long, intraProb: Double, interProb: Double, blocks: Int, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(totalVertices, intraProb, interProb, blocks, startingIndex)
    
    private def generateGraph(totalVertices: Long, intraProb: Double, interProb: Double, blocks: Int, startingIndex: Long): Map[Long, Iterable[Long]] = {
        val verticesPerBlock: Long = floor(totalVertices / blocks).toLong
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
        graph
    }
    
    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.toIterable.flatMap { case (node, neighbors) =>
        neighbors.flatMap(neighbor => List((node, neighbor), (neighbor, node)))
    }
}

class Torus2DGraph(width: Int, height: Int, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(width, height, startingIndex)
    
    private def generateGraph(width: Int, height: Int, startingIndex: Long): Map[Long, Iterable[Long]] = {
        (0L until width * height).par.map(index => {
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
        }).toMap.seq
    }

    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.toIterable.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}

class BipartiteGraph(set1Size: Long, set2Size: Long, startingIndex: Long) extends Graph {
    private val g: Map[Long, Iterable[Long]] = generateGraph(set1Size, set2Size, startingIndex)
    
    private def generateGraph(set1Size: Long, set2Size: Long, startingIndex: Long): Map[Long, Iterable[Long]] = {
        val set1 = (startingIndex until (set1Size + startingIndex))
        val set2 = ((set1Size + startingIndex) until (set1Size + startingIndex + set2Size))
        // Create edges between every node in set1 and every node in set2
        (set1.par.map(n => (n -> set2)) ++ set2.par.map(n => (n -> set1))).toMap.seq
    }

    override def adjacencyList(): Map[Long, Iterable[Long]] = g

    override def nodes: Iterable[Long] = g.keys

    override def edges: Iterable[(Long, Long)] = g.toIterable.flatMap { case (node, neighbors) =>
        neighbors.map(neighbor => (node, neighbor))
    }
}