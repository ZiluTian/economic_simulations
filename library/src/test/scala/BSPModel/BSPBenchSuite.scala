package BSPModel
package test

import org.scalatest._
import funsuite._
import java.io.{File, PrintWriter}
import scala.collection.mutable.Queue
import scala.collection.mutable.{Map => MutMap}
import scala.util.Random

trait BSPBenchSuite extends AnyFunSuite {
    val totalRounds: Int 
    val experimentName: String

    lazy val writer = {
        val x = new File(f"${experimentName}")
        if (x.exists()) {
            val version: Int = getVersion(x)    
            val versionedFile = new File(f"${experimentName}_v${version}")
            x.renameTo(versionedFile)
        }
        new PrintWriter(f"${experimentName}")
    }

    implicit def toVectorGraphLong(g: Map[Long, Iterable[Long]]): Map[Long, Vector[Long]] = {
        g.map(i => (i._1, i._2.toVector))
    } 

    implicit def toVectorGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Vector[Int]] = {
        g.map(i => (i._1.toInt, i._2.map(_.toInt).toVector))
    } 

    implicit def toGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Iterable[Int]] = {
        g.map(i => (i._1.toInt, i._2.map(_.toInt)))
    } 

    implicit def toVertexSetInt(g: Iterable[Long]): Set[Int] = g.map(_.toInt).toSet
    implicit def toVertexSetLong(g: Iterable[Long]): Set[Long] = g.toSet
    implicit def toEdgeVecLong(g: Iterable[Long]): Vector[Long] = g.toVector
    implicit def toEdgeVecInt(g: Iterable[Long]): Vector[Int] = g.map(_.toInt).toVector
    implicit def toEdgeSetInt(g: Iterable[(Long, Long)]): Set[(Int, Int)] = g.map(i => (i._1.toInt, i._2.toInt)).toSet

    def getVersion(file: File): Int = {
        val dir = Option(file.getParentFile).getOrElse(new File("."))
        val baseName = file.getName
        val versions = dir.listFiles()
            .filter(f => f.getName.startsWith(f"${baseName}_"))
            .map(f => f.getName.split("_").last.split("v").last.toInt)
        if (versions.size == 0) {
            1
        } else {
            versions.max + 1
        }
    }

    def benchmarkTool[R](writer: PrintWriter, block: => R): Unit = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        writer.write("Elapsed time: " + (t1 - t0) + "ms\n")
        writer.flush()
    }

    def benchmarkTool[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0) + "ms")
        result
    }

    // balanced partition. Each graph should have the same number of nodes 
    def partition(g: cloudcity.lib.Graph.Graph, blocks: Int): IndexedSeq[BSPModel.Graph[BSPId]] = {
        val rand = new Random()

        // computes the number of vertices in each block
        val totalVertices: Int = g.nodes.size
        val verticesPerBlock: Int = totalVertices / blocks
        val extraVertices: Int = totalVertices % blocks
    
        // allow BSPId to be of different type than default Long type in base Graph
        val adjacencyList: Map[BSPId, Iterable[BSPId]] = toGraphInt(g.adjacencyList())

        var unassigned: Set[BSPId] = g.nodes
        var unvisitedEdges: Set[(BSPId, BSPId)] = g.edges

        // println("Unassigned " + unassigned)
        // println("Unvisited edges " + unvisitedEdges)

        // BFS recursively add connected nodes to the block until reaching capacity
        def buildBlock(seed: BSPId, capacity: Int): Set[BSPId] = {
            var blockVertices = Set[BSPId](seed)
            val queue = Queue(seed)
            var blockSize: Int = 1

            while (blockSize < capacity) {
                if (queue.nonEmpty) {
                    val current = queue.dequeue
                    for (neighbor <- adjacencyList.getOrElse(current, Iterable.empty)){
                        if (blockSize < capacity) {
                            if (unassigned.contains(neighbor)) {
                                blockVertices += neighbor 
                                unassigned -= neighbor
                                unvisitedEdges -= ((current, neighbor))
                                blockSize += 1
                                queue.enqueue(neighbor)
                            }
                        } else {
                            return blockVertices
                        }
                    }
                 } else {
                    // println(f"Block size is ${blockSize} capacity is ${capacity} unassigned size ${unassigned.size}")
                    val randSeed = unassigned.head
                    unassigned -= randSeed
                    blockVertices += randSeed 
                    queue.enqueue(randSeed)
                } 
            }
            blockVertices
        }

        val capacityList: Vector[Int] = Vector.fill(extraVertices)(verticesPerBlock + 1) ++ Vector.fill(blocks - extraVertices)(verticesPerBlock)

        // create a list with remainder distributed across the first few blocks
        val partitionedVertices: Vector[Set[BSPId]] = capacityList.tail.map(i => {
            // randomly pick a starting node from unvisited nodes to build blocks
            val randSeed = unassigned.head
            unassigned -= randSeed
            buildBlock(randSeed, i)
        }) ++ Vector(unassigned)

        // assert(partitionedVertices.map(_.size).sum == totalVertices)

        val indexedVertex: Map[BSPId, Int] = partitionedVertices.zipWithIndex.flatMap(i => {
            i._1.map(j => (j, i._2))
        }).toMap
        
        // assert(indexedVertex.size == totalVertices)

        // println("Unvisited edges are " + unvisitedEdges)
        // println("Partitioned vertices are " + partitionedVertices)

        // unvisited edges are cross-partition edges
        partitionedVertices.map(x => {
            var in = MutMap[Int, Vector[BSPId]]().withDefaultValue(Vector[BSPId]())
            var out = MutMap[Int, Vector[BSPId]]().withDefaultValue(Vector[BSPId]())

            unvisitedEdges.foreach {
                case (src, dst) if x.contains(src) =>
                    val partId = indexedVertex(dst)
                    in.update(partId, in(partId) :+ dst)
                case (src, dst) if x.contains(dst) => 
                    val partId = indexedVertex(src)
                    out.update(partId, out(partId) :+ src)
                case _ => 
            }

            new BSPModel.Graph[BSPId] {
                val vertices: Set[BSPId] = x
                val edges: Map[Int, Vector[BSPId]] = Map()
                val inEdges: Map[Int, Vector[BSPId]] = in.toMap
                val outEdges: Map[Int, Vector[BSPId]] = out.toMap
            }
        })
    }
}