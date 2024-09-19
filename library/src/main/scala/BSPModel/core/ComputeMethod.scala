package BSPModel

trait ComputeMethod {
    type State
    type InMessage
    type OutMessage
    // type Delta

    def stateToMessage(s: State): OutMessage
    def partialCompute(ms: Iterable[InMessage]): Option[InMessage]
    // def partialComputeWithDelta(ms: Iterable[InMessage], delta: Delta): Delta
    // def updateStateWithDelta(s: State, delta: Delta): State
    // def updateStateWithPartialCompute(s: State, m: Option[InMessage]): State
    def updateState(s: State, m: Option[InMessage]): State
    
    def run(state: State, ms: Iterable[InMessage]): State = {
        updateState(state, partialCompute(ms))
    }
}

// trait DeltaCompute {
//     type Delta

// }
// trait StatelessComputeMethod extends ComputeMethod 

// // foldLeft allows for maintaining a state when aggregating messages, hence "stateful"
// trait StatefulComputeMethod  extends ComputeMethod {
//     // change the value of initFoldValue inplace
//     def statefulFold(ms: Iterable[InMessage]): Unit

//     override def partialCompute(ms: Iterable[InMessage]): Option[InMessage] = {
//         statefulFold(ms)
//         None
//     }

//     override def run(state: State, ms: Iterable[InMessage]): State = {
//         partialCompute(ms)   
//         updateState(state, None)
//     }
// }