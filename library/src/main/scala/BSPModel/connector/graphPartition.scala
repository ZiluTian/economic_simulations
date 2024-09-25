package BSPModel

import scala.collection.mutable.Queue
import scala.collection.mutable.{Map => MutMap}

object Connector {
    implicit def toVectorGraphLong(g: Map[Long, Iterable[Long]]): Map[Long, Vector[Long]] = g.map(i => (i._1, i._2.toVector))
    implicit def toVectorGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Vector[Int]] = g.map(i => (i._1.toInt, i._2.map(_.toInt).toVector))
    implicit def toGraphInt(g: Map[Long, Iterable[Long]]): Map[Int, Iterable[Int]] = g.map(i => (i._1.toInt, i._2.map(_.toInt)))
    implicit def toVertexSetInt(g: Iterable[Long]): Set[Int] = g.map(_.toInt).toSet
    implicit def toVertexSetLong(g: Iterable[Long]): Set[Long] = g.toSet
    implicit def toVertexVecLong(g: Iterable[Long]): Vector[Long] = g.toVector
    implicit def toVertexVecInt(g: Iterable[Long]): Vector[Int] = g.map(_.toInt).toVector
    implicit def toEdgeSetInt(g: Iterable[(Long, Long)]): Set[(Int, Int)] = g.map(i => (i._1.toInt, i._2.toInt)).toSet

    // return the partition that contains edges in g for a given partition map
    def partitionPartialGraph(edges: Iterable[(BSPId, BSPId)], nodes: Set[BSPId], indexedVertex: Map[BSPId, Int]): IndexedSeq[BSPModel.Graph[BSPId]] = {
        // assert(indexedVertex.size == totalVertices)
        // println("Unvisited edges are " + unvisitedEdges)
        // println("Partitioned vertices are " + partitionedVertices)
        // unvisited edges are cross-partition edges

        nodes.groupBy(indexedVertex(_)).map(i => {                        
            var in = MutMap[Int, Set[BSPId]]().withDefaultValue(Set[BSPId]())
            var out = MutMap[Int, Set[BSPId]]().withDefaultValue(Set[BSPId]())

            edges.foreach {
                case (src, dst) if ((indexedVertex(src) == i._1) && (indexedVertex(dst) != i._1)) =>
                    val partId = indexedVertex(dst)
                    out.update(partId, out(partId)+src)
                case (src, dst) if ((indexedVertex(dst) == i._1) && (indexedVertex(src) != i._1)) => 
                    val partId = indexedVertex(src)
                    in.update(partId, in(partId)+src)
                case _ => 
            }
            
            new BSPModel.Graph[BSPId] {
                val vertices: Set[BSPId] = i._2
                val edges: Map[Int, Vector[BSPId]] = Map()
                val inExtVertices: Map[Int, Vector[BSPId]] = in.map(i => (i._1, i._2.toVector.sorted)).toMap
                val outIntVertices: Map[Int, Vector[BSPId]] = out.map(i => (i._1, i._2.toVector.sorted)).toMap
            }
        }).toVector
    }

    // balanced partition. Each graph should have the same number of nodes 
    def partition(g: cloudcity.lib.Graph.Graph, blocks: Int): IndexedSeq[BSPModel.Graph[BSPId]] = {
        if (blocks <= 1) {
            return Vector(new BSPModel.Graph[BSPId]{
                val vertices: Set[BSPId] = g.nodes
                val edges: Map[Int, Vector[BSPId]] = Map()
                val inExtVertices: Map[Int, Vector[BSPId]] = Map()
                val outIntVertices: Map[Int, Vector[BSPId]] = Map()
            })
        }
        
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

            while (blockVertices.size < capacity) {
                if (queue.nonEmpty) {
                    val current = queue.dequeue
                    for (neighbor <- adjacencyList.getOrElse(current, Iterable.empty)){
                        if (blockVertices.size < capacity) {
                            if (unassigned.contains(neighbor)) {
                                blockVertices += neighbor 
                                unassigned -= neighbor
                                // Does not throw error in case not found
                                unvisitedEdges -= ((current, neighbor))
                                unvisitedEdges -= ((neighbor, current))
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

        // println(f"Parititioned vertices are $partitionedVertices")
        // println(f"Edges in the original graph are ${g.edges}")

        // assert(partitionedVertices.map(_.size).sum == totalVertices)

        val indexedVertex: Map[BSPId, Int] = partitionedVertices.zipWithIndex.flatMap(i => {
            i._1.map(j => (j, i._2))
        }).toMap
        
        // assert(indexedVertex.size == totalVertices)
        // println("Unvisited edges are " + unvisitedEdges)
        // println("Partitioned vertices are " + partitionedVertices)

        // unvisited edges are cross-partition edges
        partitionedVertices.map(x => {
            var in = MutMap[Int, Set[BSPId]]().withDefaultValue(Set[BSPId]())
            var out = MutMap[Int, Set[BSPId]]().withDefaultValue(Set[BSPId]())

            unvisitedEdges.foreach {
                case (src, dst) if x.contains(src) && !x.contains(dst) =>
                    val partId = indexedVertex(dst)
                    out.update(partId, out(partId)+src)
                case (src, dst) if x.contains(dst) && !x.contains(src) => 
                    val partId = indexedVertex(src)
                    in.update(partId, in(partId)+src)
                case _ => 
            }
            
            new BSPModel.Graph[BSPId] {
                val vertices: Set[BSPId] = x
                val edges: Map[Int, Vector[BSPId]] = Map()
                val inExtVertices: Map[Int, Vector[BSPId]] = in.map(i => (i._1, i._2.toVector.sorted)).toMap
                val outIntVertices: Map[Int, Vector[BSPId]] = out.map(i => (i._1, i._2.toVector.sorted)).toMap
            }
        })
    }
}