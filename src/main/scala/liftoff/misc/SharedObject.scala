package liftoff.misc

import com.sun.jna.NativeLibrary

import java.io.File
import scala.sys.process.Process

class SharedObject(libFile: File) {

  def load(): NativeLibrary = {
    if (!libFile.exists()) {
      throw new RuntimeException(
        s"Shared object file ${libFile.getAbsolutePath} does not exist."
      )
    }
    NativeLibrary.getInstance(libFile.getAbsolutePath)
  }

}

object SharedObject {

  def sharedLibraryExtension: String = {
    if (System.getProperty("os.name").toLowerCase.contains("windows")) ".dll"
    else if (System.getProperty("os.name").toLowerCase.contains("mac")) ".dylib"
    else ".so"
  }

  def createRecipe(
      libname: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      options: Seq[String] = Seq.empty
  ): WorkingDirectory.Recipe[SharedObject] = {

    val libFile = dir / (libname + sharedLibraryExtension)

    val command = Seq("g++", "-shared", "-fPIC", "-o", libFile.getAbsolutePath()) ++
      options ++
      sources.map(_.getAbsolutePath)

    dir.addRecipe(
      Seq(libFile),
      sources,
      command,
      fs => new SharedObject(fs.head)
    )

  }

}

