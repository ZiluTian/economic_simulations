package meta.compile 

object compileSims {
  import meta.classLifting.Lifter
  import meta.deep.IR.TopLevel._
  import meta.deep.codegen.{CreateActorGraphs, CreateCode, EdgeMerge, Pipeline, StateMachineElement, ActorMerge, SSO}
  import meta.deep.runtime.Actor

  def apply(startClasses: List[Clasz[_ <: Actor]], mainClass: Clasz[_], mode: CompilationMode = Vanilla, destFolder: String=""): Unit = {

    val lifter = new Lifter()
    val simulationData = lifter(startClasses, mainClass)

    var statemachineElements: List[StateMachineElement] = List(new EdgeMerge())

    val canonicalName: String = mainClass.getClass.getPackage.getName()

    val pkgName: String = mode.pkgName(canonicalName) 

    statemachineElements = mode match {
      case Vanilla => 
        statemachineElements
      case SimsMerge(namePairs) =>
        new ActorMerge(namePairs) :: statemachineElements
      case SimsStateless(statelessServers) =>
        new SSO(statelessServers) :: statemachineElements
    }

    val destFolderName = destFolder match {
      case "" => "example/src/main/scala/generated/" + canonicalName
      case s => s 
    }
    statemachineElements = statemachineElements :+ new CreateCode(simulationData._2,
      destFolderName, 
      pkgName)

    val pipeline = Pipeline(new CreateActorGraphs(simulationData._1), statemachineElements)

    pipeline.run()
  }
}