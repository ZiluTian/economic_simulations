package BSPModel

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
case class ArrayGraph[NodeId](
    val g: Graph[NodeId],
    val inCache: Map[PartitionId, Array[Any]],
) extends Graph[NodeId] {
    val vertices = g.vertices
    val edges = g.edges
    val inExtVertices = g.inExtVertices
    val outIntVertices = g.outIntVertices

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
        ArrayGraph[NodeId](g, 
            g.inExtVertices.map(i => {
                (i._1, new Array[Any](i._2.size))
            })
        )
    }
}