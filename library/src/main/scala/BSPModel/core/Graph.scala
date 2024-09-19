package BSPModel

import scala.collection.mutable.{Map => MutMap}

trait Graph[NodeId] {
    // local vertices
    val vertices: Set[NodeId]
    // transitive closure of internal vertices
    val edges: Map[NodeId, Vector[NodeId]]
    // synthesized message encoding for incoming messages
    val inExtVertices: Map[PartitionId, Vector[NodeId]]
    val outIntVertices: Map[PartitionId, Vector[NodeId]]
}

// Pad topo with cache to buffer messages
abstract class ArrayGraph[NodeId]() extends Graph[NodeId] {
    val vertices: Set[NodeId] 
    val edges: Map[NodeId, Vector[NodeId]]
    val inExtVertices: Map[PartitionId, Vector[NodeId]]
    val outIntVertices: Map[PartitionId, Vector[NodeId]]
    val inCache: MutMap[PartitionId, Vector[_ <: Any]]

    def getInboxCacheIndex(i: NodeId): Option[(PartitionId, Int)] = {
        // return the index of a value in a cache
        inExtVertices.collectFirst {
            case (key, list) if list.contains(i) =>
            (key, list.indexOf(i))
        }
    }
}

object ArrayGraph{
    def fromGraph[NodeId](g: Graph[NodeId]): ArrayGraph[NodeId] = {
        new ArrayGraph[NodeId](){
            val vertices = g.vertices
            val edges = g.edges
            val inExtVertices = g.inExtVertices
            val outIntVertices = g.outIntVertices
            val inCache = MutMap[PartitionId, Vector[_ <: Any]]()
        }
    }
}