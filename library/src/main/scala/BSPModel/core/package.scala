package object BSPModel {
    type BSPId = Int
    type PartitionId = Int

    // def debug(msg: () => String) = {
    //     println(f"Debug: ${msg()}")
    // }

    def partitionIdToBSPId(pid: PartitionId): BSPId = {
        pid
    }

    private var lastId: BSPId = 0

    def getNextId(): BSPId = this.synchronized {
        lastId = lastId + 1
        lastId
    }
}