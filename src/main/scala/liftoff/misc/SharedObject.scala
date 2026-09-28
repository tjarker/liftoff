package liftoff.misc

import java.lang.foreign._

import java.io.File
import scala.sys.process.Process
import java.lang.invoke.MethodHandle

private[liftoff] class SharedObject(libFile: File) {

  def load(): Library = {
    if (!libFile.exists()) {
      throw new RuntimeException(
        s"Shared object file ${libFile.getAbsolutePath} does not exist."
      )
    }
    val arena = Arena.ofShared()
    val lookup = SymbolLookup.libraryLookup(libFile.toPath(), arena)
    val linker = Linker.nativeLinker()
    new Library(lookup, linker)
  }

}

private[liftoff] class Library(lookup: SymbolLookup, linker: Linker) {

  def functionHandle(name: String, descriptor: FunctionDescriptor): MethodHandle = {
    linker.downcallHandle(lookup.find(name).get, descriptor)
  }
}

private[liftoff] object SharedObject {

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

  /** Runs `command`, a complete link command producing `libFile` from `deps`. */
  def createRecipe(
      libFile: File,
      dir: WorkingDirectory,
      deps: Seq[File],
      command: Seq[String]
  ): WorkingDirectory.Recipe[SharedObject] = {
    dir.addRecipe(
      Seq(libFile),
      deps,
      command,
      fs => new SharedObject(fs.head)
    )
  }

}

