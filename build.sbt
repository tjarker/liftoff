name := "liftoff"
version := "0.0.1"
scalaVersion := "2.13.12"
organization := "io.github.tjarker"

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  "-Ymacro-annotations"
)

javaOptions ++= Seq(
  "-Djdk.virtualThreadScheduler.parallelism=1",
  "-Djdk.virtualThreadScheduler.maxPoolSize=1",
  "-Djdk.virtualThreadScheduler.minRunnable=1"
)

fork := true
javaOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"
javacOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED"

libraryDependencies += "net.java.dev.jna" % "jna" % "5.13.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

val chiselVersion = "6.0.0"
addCompilerPlugin(
  "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
)
libraryDependencies += "org.chipsalliance" %% "chisel" % chiselVersion