package liftoff.misc

import java.io.File
import scala.collection.mutable.ArrayBuffer
import liftoff.simulation.Time

object WorkingDirectory {
  def apply(dir: String): WorkingDirectory = {
    val dirFile = new File(dir)
    dirFile.mkdirs()
    if (!dirFile.isDirectory) {
      throw new IllegalArgumentException(s"Path $dir is not a directory.")
    }
    new WorkingDirectory(dirFile)
  }

  class Recipe[T](
      dir: WorkingDirectory,
      targets: Seq[File],
      mapper: Seq[File] => T
  ) {

    def invoke(): T = {
      require(targets.nonEmpty, "No targets specified for the recipe.")
      val commands = "make" +: targets.map(_.getAbsolutePath())
      val process = new ProcessBuilder(commands: _*)
        .directory(dir.dir)
        .redirectErrorStream(true)
        .start()
      scala.io.Source
        .fromInputStream(process.getInputStream)
        .getLines()
        .foreach(Reporting.info(Time(0, Time.TimeUnit.s), "Makefile", _))
      val exitCode = process.waitFor()
      if (exitCode != 0) {
        throw new RuntimeException(
          s"Recipe failed with exit code $exitCode. Command: ${commands.mkString(" ")}"
        )
      }
      targets.foreach { target =>
        if (!target.exists()) {
          throw new RuntimeException(
            s"Target $target was not created by the recipe."
          )
        }
      }
      mapper(targets)
    }
  }
}

class WorkingDirectory(val dir: File) {

  val artifacts = new ArrayBuffer[File]()
  val subdirs = new ArrayBuffer[WorkingDirectory]()

  def path: String = dir.getAbsolutePath

  val makefile = this / "Makefile"
  // clear the Makefile and create a new one
  java.nio.file.Files.write(
    makefile.toPath(),
    "# Automatically Generated\n".getBytes("UTF-8"),
    java.nio.file.StandardOpenOption.CREATE,
    java.nio.file.StandardOpenOption.TRUNCATE_EXISTING
  )

  def addRecipe[T](
      targets: Seq[File],
      deps: Seq[File],
      commands: Seq[String],
      mapper: Seq[File] => T
  ): WorkingDirectory.Recipe[T] = {
    val targetPaths = targets.map(_.getAbsolutePath()).mkString(" ")
    val depsPaths = deps.map(_.getAbsolutePath).mkString(" ")
    val commandsStr = commands.mkString(" \\\n\t  ")
    val makefileContent =
      s"""$targetPaths: $depsPaths Makefile
               |\t$commandsStr
               |""".stripMargin
    java.nio.file.Files.write(
      makefile.toPath,
      makefileContent.getBytes("UTF-8"),
      java.nio.file.StandardOpenOption.APPEND
    )
    new WorkingDirectory.Recipe(this, targets, mapper)
  }

  def addFile(name: String, content: String): File = {
    val file = new File(dir, name)

    // check if content is the same for existing file
    if (file.exists() && file.isFile) {
      val existingContent = scala.io.Source.fromFile(file).mkString
      if (existingContent == content) {
        return file // No need to write if content is the same
      }
    }

    file.getParentFile.mkdirs()
    java.nio.file.Files.write(
      file.toPath,
      content.getBytes("UTF-8"),
      java.nio.file.StandardOpenOption.CREATE,
      java.nio.file.StandardOpenOption.TRUNCATE_EXISTING
    )
    artifacts += file
    file
  }

  def addSubDir(file: File): WorkingDirectory = {
    file.mkdirs()
    val subdir = new WorkingDirectory(file)
    subdirs += subdir
    subdir
  }

  def addArtifact(file: File): Unit = {
    artifacts += file
  }

  def /(name: String): File = {
    val file = new File(dir, name)
    file
  }

  def deleteArtifact(file: File): Unit = {
    if (file.exists() && !file.isDirectory) {
      file.delete()
    }
  }

  def clean(): Unit = {
    artifacts.foreach(deleteArtifact)
    subdirs.foreach(_.delete())
    artifacts.clear()
  }

  def delete(): Unit = {
    clean()
    dir.delete()
  }

}
