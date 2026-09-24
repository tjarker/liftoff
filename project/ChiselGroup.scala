import sbt._

/** Chisel releases that are binary compatible with each other.
  *
  * liftoff is built and published once per group. Each build is compiled against the oldest
  * release of its group, so the published artifact works with every release of the group.
  *
  * @param projectId    sbt project id, also used in the artifact name
  * @param organization Maven group of the Chisel artifacts
  * @param library      Chisel artifact name; its compiler plugin is `<library>-plugin`
  * @param version      oldest release of the group
  * @param scalaVersion newest Scala version the compiler plugin of `version` was published for
  */
case class ChiselGroup(
  projectId: String,
  organization: String,
  library: String,
  version: String,
  scalaVersion: String,
) {

  /** The releases of the group, such as "3.6.x" or "7.x". */
  def releases: String =
    version.split('.').take(if (library == "chisel3") 2 else 1).mkString("", ".", ".x")

  /** This group with its Chisel and Scala version replaced by the environment variables
    * `<PROJECTID>_CHISEL` and `<PROJECTID>_SCALA`, if set (for example `CHISEL7_CHISEL`). CI uses
    * them to also test against the newest release of each group; releases never set them.
    */
  def fromEnvironment: ChiselGroup = {
    val prefix = projectId.toUpperCase
    copy(
      version = sys.env.getOrElse(s"${prefix}_CHISEL", version),
      scalaVersion = sys.env.getOrElse(s"${prefix}_SCALA", scalaVersion),
    )
  }

  def dependency: ModuleID = organization %% library % version

  def compilerPlugin: ModuleID =
    organization % s"$library-plugin" % version cross CrossVersion.full
}

object ChiselGroup {
  val chisel36 = ChiselGroup("chisel36", "edu.berkeley.cs", "chisel3", "3.6.1", "2.13.14")
  val chisel6 = ChiselGroup("chisel6", "org.chipsalliance", "chisel", "6.0.0", "2.13.12")
  val chisel7 = ChiselGroup("chisel7", "org.chipsalliance", "chisel", "7.0.0", "2.13.16")
}
