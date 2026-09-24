// liftoff is built once per Chisel compatibility group (see project/ChiselGroup.scala).
// Every group compiles the shared sources in src/<scope>/scala plus its own Chisel dependent code
// in src/<scope>/scala-<group>; the root project only aggregates the groups.
// Run `sbt chisel7/test` for a single group.

ThisBuild / version := "0.0.1"
ThisBuild / organization := "io.github.tjarker"
ThisBuild / scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
  "-Ymacro-annotations"
)

def sourceDirs(root: File, scope: String, group: ChiselGroup): Seq[File] =
  Seq(root / "src" / scope / "scala", root / "src" / scope / s"scala-${group.projectId}")

// The macros have to be compiled before the code using them, so they get a project of their own,
// built with the Scala version of the group.
def macrosProject(baseGroup: ChiselGroup): Project = {
  val group = baseGroup.fromEnvironment
  Project(s"macros-${group.projectId}", file("builds") / s"macros-${group.projectId}")
    .settings(
      scalaVersion := group.scalaVersion,
      libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      Compile / unmanagedSourceDirectories := Seq((ThisBuild / baseDirectory).value / "macros" / "src" / "main" / "scala"),
      publish / skip := true,
    )
}

def groupProject(baseGroup: ChiselGroup, macros: Project): Project = {
  val group = baseGroup.fromEnvironment
  Project(group.projectId, file("builds") / group.projectId)
    .dependsOn(macros % "compile-internal->compile;test-internal->compile")
    .settings(
      name := s"liftoff-${group.projectId}",
      description := s"Hardware verification framework for Chisel ${group.releases}",
      scalaVersion := group.scalaVersion,
      libraryDependencies ++= Seq(
        group.dependency,
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "net.java.dev.jna" % "jna" % "5.13.0",
        "org.scala-sbt" % "test-interface" % "1.0",
        "com.lihaoyi" %% "fansi" % "0.5.0",
        "com.lihaoyi" %% "sourcecode" % "0.4.2",
        "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      ),
      addCompilerPlugin(group.compilerPlugin),
      Compile / unmanagedSourceDirectories :=
        sourceDirs((ThisBuild / baseDirectory).value, "main", group) :+ (ThisBuild / baseDirectory).value / "src" / "main" / "java",
      Test / unmanagedSourceDirectories := sourceDirs((ThisBuild / baseDirectory).value, "test", group),
      // User code expands the macros, so they ship inside the liftoff jar.
      Compile / packageBin / mappings ++= (macros / Compile / packageBin / mappings).value,
      Compile / packageSrc / mappings ++= (macros / Compile / packageSrc / mappings).value,
      javacOptions += "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED",
      fork := true,
      javaOptions ++= Seq(
        "-Djdk.virtualThreadScheduler.parallelism=1",
        "-Djdk.virtualThreadScheduler.maxPoolSize=1",
        "-Djdk.virtualThreadScheduler.minRunnable=1",
        "--add-exports=java.base/jdk.internal.vm=ALL-UNNAMED",
        "--enable-native-access=ALL-UNNAMED",
      ),
    )
}

lazy val macrosChisel36 = macrosProject(ChiselGroup.chisel36)
lazy val chisel36 = groupProject(ChiselGroup.chisel36, macrosChisel36)

lazy val macrosChisel6 = macrosProject(ChiselGroup.chisel6)
lazy val chisel6 = groupProject(ChiselGroup.chisel6, macrosChisel6)

lazy val macrosChisel7 = macrosProject(ChiselGroup.chisel7)
lazy val chisel7 = groupProject(ChiselGroup.chisel7, macrosChisel7)

lazy val root = (project in file("."))
  .aggregate(chisel36, chisel6, chisel7)
  .settings(
    name := "liftoff",
    scalaVersion := ChiselGroup.chisel7.scalaVersion,
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := Nil,
    publish / skip := true,
  )
