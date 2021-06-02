ThisBuild / organization := "ch.epfl.data"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "1.0"

val paradiseVersion = "2.1.0"
val breezeVersion = "0.13.2"
val scalaTestVersion = "3.0.0"
val squidVersion = "0.4.1-SNAPSHOT"
val sparkVersion = "3.0.1"
val graphVizVersion = "0.10.0"
val akkaVersion = "2.6.14"

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  libraryDependencies += "org.scalanlp" %% "breeze" % breezeVersion,
  libraryDependencies += "org.scalanlp" %% "breeze-natives" % breezeVersion,
  libraryDependencies += "org.scalanlp" %% "breeze-viz" % breezeVersion,
)

lazy val squidSettings = Seq(
  libraryDependencies += "ch.epfl.data" %% "squid" % squidVersion, 
  resolvers += Resolver.sonatypeRepo("snapshots"),
  autoCompilerPlugins := true,
  addCompilerPlugin(
    "org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full
  )
)

lazy val akkaSettings = Seq(
  libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
  libraryDependencies += "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
)

lazy val sparkSettings = Seq(
  libraryDependencies += "org.apache.spark" %% "spark-core" % sparkVersion,
  libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion,
)

// Enable graph drawing when debugging
lazy val graphSettings = Seq(
  libraryDependencies += "guru.nidi" % "graphviz-java" % graphVizVersion,
)

lazy val custMacros = (project in file("custMacros"))
  .settings(
    name := "custMacros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val root = (project in file("core"))
  .settings(
    name := "root",
    commonSettings, squidSettings, graphSettings, sparkSettings, akkaSettings
  )

lazy val library = (project in file("lib"))
  .settings(
    name := "library",
    commonSettings, squidSettings
  )
  .dependsOn(root)

lazy val runAll = taskKey[Unit]("run-all, for compiling all meta examples")

def runAllIn(config: Configuration) = Def.task {
    val s = streams.value
    val cp = (fullClasspath in config).value
    val r = (runner in run).value
    (discoveredMainClasses in config).value.foreach(c =>
      r.run(c, cp.files, Seq(), s.log))
}

lazy val example = (project in file("example"))
  .settings(
    name := "example",
    commonSettings, squidSettings,
    runAll := runAllIn(Compile).value
  )
  .dependsOn(root, library, custMacros)

lazy val genExample = (project in file("generated"))
  .settings(
    name := "genExample",
    commonSettings, sparkSettings, akkaSettings,
  )
  .dependsOn(root, library, example)


