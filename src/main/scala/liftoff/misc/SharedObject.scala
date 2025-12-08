package liftoff.misc

import com.sun.jna._

import java.io.File
import scala.sys.process.Process

class SharedObject(libFile: File) {

  def load(): NativeLibrary = {
    if (!libFile.exists()) {
      throw new RuntimeException(
        s"Shared object file ${libFile.getAbsolutePath} does not exist."
      )
    }
    val opts = new java.util.HashMap[String, Int]()
    opts.put(Library.OPTION_OPEN_FLAGS, 2)
    NativeLibrary.getInstance(libFile.getAbsolutePath, opts)
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

    val nonLibOptions = options.filterNot(_.startsWith("-l"))
    val libOptions = options.filter(_.startsWith("-l"))

    val command = Seq("g++", "-shared", "-fPIC", "-o", libFile.getAbsolutePath()) ++
      nonLibOptions ++
      sources.map(_.getAbsolutePath) ++
      libOptions
    dir.addRecipe(
      Seq(libFile),
      sources,
      command,
      fs => new SharedObject(fs.head)
    )

  }

}

