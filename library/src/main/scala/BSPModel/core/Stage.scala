package BSPModel

trait Stage {
    this: BSP with ComputeMethod =>

    // cascaded staging, allow for multiple staged exprs
    val stagedComputation: List[StagedExpr]

    override def run(ms: Iterable[InMessage]): Unit = {
        val messages = stagedComputation.map(c => {
            c.compile().asInstanceOf[Option[InMessage]]
        }).filter(m => !m.isEmpty).map(m => m.get)
        state = run(state, messages ++ ms)
    }
}