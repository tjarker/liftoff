package liftoff.simulation.verilator

import liftoff.simulation._
import liftoff.misc.SharedObject
import liftoff.misc.WorkingDirectory
import liftoff.wordArrayOps
import liftoff.bigIntOps
import liftoff.simulation.Time._

import scala.collection.mutable

import java.io.File
import liftoff.misc.Reporting

import java.lang.foreign._

private[liftoff] object VerilatorSimModelFactory {

  val uniqueNameCounter = mutable.Map[String, Int]()
  def uniqueName(base: String): String = {
    val count = uniqueNameCounter.getOrElseUpdate(base, 0)
    uniqueNameCounter(base) = count + 1
    s"${base}_$count"
  }

  val buildDirs = mutable.Set[WorkingDirectory]()

  def create(
      topName: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      build: VerilatorBuild = VerilatorBuild()
  ): VerilatorSimModelFactory = {

    checkSetup()

    if (buildDirs.contains(dir)) {
      throw new Exception(s"Working dir for $topName (${dir.dir.getAbsolutePath()}) has already been used in this run.")
    }
    buildDirs += dir

    val verilatorDir = dir.addSubDir(dir / "verilator")
    val verilogSources = sources ++ build.sources

    val verilateCommand = build.verilator(
      Seq("verilator") ++ (Seq(
        Verilator.Arguments.OptimizationLevel("3"),
        Verilator.Arguments.CFlags("-fPIC -fpermissive -O3")
      ) ++ build.arguments
        // Generated files may include each other by name (Chisel 7 layers, for example).
        ++ verilogSources.map(_.getAbsoluteFile.getParent).distinct.map(Verilator.Arguments.Include(_)))
        .flatMap(_.toStrings) ++
        (verilogSources ++ build.dpi).map(_.getAbsolutePath)
    ) ++ (
      Seq(Verilator.Arguments.CC, Verilator.Arguments.Build) ++
        build.waves.argument ++
        Seq(Verilator.Arguments.BuildDir(verilatorDir.path), Verilator.Arguments.TopModule(topName))
    ).flatMap(_.toStrings)

    // Verilator's own make also compares timestamps, so all of its objects have to go.
    rebuildOnChange(verilatorDir, "verilate", verilateCommand)(
      verilatorDir.dir.listFiles().toSeq.filter(f => f.getName.endsWith(".o") || f.getName.endsWith(".a"))
    )

    val verilateRecipe = Verilator.createRecipe(
      verilatorDir,
      topName,
      verilateCommand,
      verilogSources ++ build.dpi,
      build.waves,
      build.dpi
    )

    val artifacts = verilateRecipe.invoke()

    val portDescriptors = PortCollector.collectPorts(verilatorDir / s"V${topName}.h")

    // Reporting.debug(None, "Verilator", s"Collected ports:\n - ${portDescriptors.mkString("\n - ")}")

    val functionPrefix = uniqueName(topName)

    val harnessFile = VerilatorModelHarness.writeHarness(
      dir,
      topName,
      functionPrefix,
      portDescriptors,
      build.waves
    )

    val harnessObject = dir / s"${functionPrefix}_harness.o"
    val harnessCommand = build.cxx(
      Seq("g++", "-I.") ++
        Verilator.getIncludeDir().get.map(p => s"-I$p") ++
        Seq("-fPIC", "-O3", "-fpermissive")
    ) ++ Seq("-c", "-o", harnessObject.getAbsolutePath(), harnessFile.getAbsolutePath())

    rebuildOnChange(verilatorDir, "harness", harnessCommand)(Seq(harnessObject))

    val harnessCompileRecipe = verilatorDir.addRecipe(
      Seq(harnessObject),
      Seq(harnessFile),
      harnessCommand,
      _.head
    )

    val compiledHarness = harnessCompileRecipe.invoke()

    val extraCOptions =
      if (System.getProperty("os.name").toLowerCase.contains("windows")) Seq()
      else if (System.getProperty("os.name").toLowerCase.contains("mac")) Seq()
      else Seq("-pthread", "-lpthread", "-latomic")

    val objects = artifacts :+ compiledHarness
    val libFile = dir / (s"lib${functionPrefix}" + SharedObject.sharedLibraryExtension)
    val linkCommand = build.link(
      Seq("g++", "-shared", "-fPIC") ++ objects.map(_.getAbsolutePath) ++
        Option.when(build.waves == Verilator.TraceFormat.Fst && Verilator.fstNeedsLz4)("-llz4") ++
        Seq("-lz") ++ extraCOptions
    ) ++ Seq("-o", libFile.getAbsolutePath())

    rebuildOnChange(dir, "link", linkCommand)(Seq(libFile))

    val sharedObjectRecipe = SharedObject.createRecipe(
      libFile,
      dir,
      objects,
      linkCommand
    )

    val sharedObject = sharedObjectRecipe.invoke()

    new VerilatorSimModelFactory(
      topName,
      functionPrefix,
      portDescriptors,
      sharedObject,
      build.waves,
      Seq(verilateCommand, harnessCommand, linkCommand)
    )
  }

  /** Fails early with a clear message when this machine cannot build or run models. */
  private def checkSetup(): Unit = {
    val jdk = Runtime.version().feature()
    if (jdk < 22) throw new IllegalStateException(s"liftoff needs JDK 22 or newer, but runs on JDK $jdk")
    if (Verilator.getExecutable.isEmpty)
      throw new IllegalStateException("liftoff needs Verilator to build models, but `verilator` is not on the PATH")
  }

  /** Records the `command` of a build step in `<step>.cmd` and, if it differs from the command recorded by the last
    * build, deletes the `outputs` of the step so that it runs again. make would miss the change: it compares
    * timestamps, and two builds can happen within its resolution.
    */
  private def rebuildOnChange(
      dir: WorkingDirectory,
      step: String,
      command: Seq[String]
  )(outputs: => Seq[File]): Unit = {
    val record = dir / s"$step.cmd"
    val recorded = command.mkString(" ") + "\n"
    val previous = if (record.exists()) Some(java.nio.file.Files.readString(record.toPath)) else None
    if (!previous.contains(recorded)) {
      outputs.foreach(_.delete())
      dir.addFile(s"$step.cmd", recorded)
    }
  }

}

/** How a Verilator model is built.
  *
  * `verilator`, `cxx` and `link` receive the complete command liftoff would run for their build step, program first,
  * and return the command to run instead. The commands are run by make, so they go through the shell. Afterwards
  * liftoff appends what the harness relies on: `--cc --build`, the flag of `waves`, `--Mdir` and `--top-module` to
  * Verilator, `-c -o <harness>.o <harness>.cpp` to the harness compilation and `-o <library>` to the link.
  *
  * @param waves
  *   format of the waves the model records
  * @param arguments
  *   Verilator arguments, part of the command the `verilator` hook receives
  * @param sources
  *   additional Verilog sources
  * @param dpi
  *   C++ sources that Verilator compiles and liftoff links into the model
  */
case class VerilatorBuild(
    waves: Verilator.TraceFormat = Verilator.TraceFormat.Fst,
    arguments: Seq[Verilator.Argument] = Seq(),
    sources: Seq[File] = Seq(),
    dpi: Seq[File] = Seq(),
    verilator: Seq[String] => Seq[String] = identity,
    cxx: Seq[String] => Seq[String] = identity,
    link: Seq[String] => Seq[String] = identity
)

private[liftoff] class VerilatorSimModelFactory(
    val name: String,
    val functionPrefix: String,
    val ports: Seq[VerilatorPortDescriptor],
    val libFile: SharedObject,
    val waves: Verilator.TraceFormat,
    /** The commands that built the model: Verilator, the harness compilation and the link. */
    val commands: Seq[Seq[String]]
) {

  val lib = libFile.load()

  import ValueLayout._

  val createContextHandle = lib.functionHandle(
    VerilatorModelHarness.createContextFunName(functionPrefix),
    FunctionDescriptor.of(
      ADDRESS, // returns a pointer to the context
      ADDRESS, // wave file path
      ADDRESS, // time unit
      ADDRESS, // args
      JAVA_INT // num args
    )
  )
  val deleteContextHandle =
    lib.functionHandle(VerilatorModelHarness.deleteContextFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS))
  val evalHandle =
    lib.functionHandle(VerilatorModelHarness.evalFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS))
  val tickHandle =
    lib.functionHandle(VerilatorModelHarness.tickFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG))
  val getPointerHandle =
    lib.functionHandle(
      VerilatorModelHarness.getPointerFunName(functionPrefix),
      FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_LONG)
    )

  def createModel(dir: WorkingDirectory): VerilatorSimModel = {
    new VerilatorSimModel(name, ports, this, dir)
  }

}

private[liftoff] class VerilatorSimModel(
    val name: String,
    val portDescriptors: Seq[VerilatorPortDescriptor],
    val factory: VerilatorSimModelFactory,
    val dir: WorkingDirectory
) extends SimModel {

  val waveFile: File = dir / s"wave.${factory.waves.fileExtension}"

  val arena = Arena.ofShared()
  val allocTraceFileName = arena.allocateFrom(waveFile.getAbsolutePath())
  val allocTimeUnit = arena.allocateFrom("1ns")

  // Create a context for the model
  val contextPtr: MemorySegment = factory.createContextHandle.invokeExact(
    allocTraceFileName,
    allocTimeUnit,
    MemorySegment.NULL, // no args
    0
  )

  val ports: Seq[VerilatorPortHandle] = portDescriptors.map(VerilatorPortHandle(this, _))

  override def inputs: Seq[InputPortHandle] = ports.collect { case i: InputPortHandle =>
    i
  }

  override def outputs: Seq[OutputPortHandle] = ports.collect { case o: OutputPortHandle =>
    o
  }

  override def getInputPortHandle(portName: String): Option[InputPortHandle] = ports.collectFirst {
    case p: InputPortHandle if p.name == portName => p
  }

  override def getOutputPortHandle(portName: String): Option[OutputPortHandle] = ports.collectFirst {
    case p: OutputPortHandle if p.name == portName => p
  }

  override def evaluate(): Unit = {
    factory.evalHandle.invokeExact(contextPtr)
  }

  override def tick(delta: Time.RelativeTime): Unit = {
    factory.tickHandle.invokeExact(contextPtr, delta.valueFs)
  }

  override def cleanupCall(): Unit = {
    factory.deleteContextHandle.invokeExact(contextPtr)
  }

}

private[liftoff] trait VerilatorPortDescriptor {
  def name: String
  def id: Int
  def width: Int
}
private[liftoff] object VerilatorPortDescriptor {
  def unapply(desc: VerilatorPortDescriptor): Option[(String, Int, Int)] = {
    Some((desc.name, desc.id, desc.width))
  }
}
private[liftoff] case class VerilatorInputDescriptor(
    name: String,
    id: Int,
    width: Int
) extends VerilatorPortDescriptor

private[liftoff] case class VerilatorOutputDescriptor(
    name: String,
    id: Int,
    width: Int
) extends VerilatorPortDescriptor

private[liftoff] trait VerilatorPortHandle extends PortHandle {
  def id: Int
  def model: VerilatorSimModel
  val address: MemorySegment = model.factory.getPointerHandle.invokeExact(model.contextPtr, id.toLong)
  def get(): BigInt
  val mask = (BigInt(1) << width) - 1
}

private[liftoff] object VerilatorPortHandle {
  def unapply(handle: VerilatorPortHandle): Option[(String, Int, Int)] = {
    Some(
      (
        handle.model.name,
        handle.id,
        handle match {
          case i: InputPortHandle  => i.width
          case o: OutputPortHandle => o.width
          case _                   => 0
        }
      )
    )
  }

  def apply(model: VerilatorSimModel, port: VerilatorPortDescriptor): VerilatorPortHandle = {
    port match {
      case VerilatorInputDescriptor(name, id, width) =>
        val mem: MemorySegment = model.factory.getPointerHandle.invokeExact(model.contextPtr, id.toLong)
        if (width <= 8) new VerilatorU8InputPortHandle(model, name, id, width, mem)
        else if (width <= 16) new VerilatorU16InputPortHandle(model, name, id, width, mem)
        else if (width <= 32) new VerilatorU32InputPortHandle(model, name, id, width, mem)
        else if (width <= 64) new VerilatorU64InputPortHandle(model, name, id, width, mem)
        else new VerilatorWideInputPortHandle(id, model, name, width, mem)
      case VerilatorOutputDescriptor(name, id, width) =>
        val mem: MemorySegment = model.factory.getPointerHandle.invokeExact(model.contextPtr, id.toLong)
        if (width <= 8) new VerilatorU8OutputPortHandle(model, name, id, width, mem)
        else if (width <= 16) new VerilatorU16OutputPortHandle(model, name, id, width, mem)
        else if (width <= 32) new VerilatorU32OutputPortHandle(model, name, id, width, mem)
        else if (width <= 64) new VerilatorU64OutputPortHandle(model, name, id, width, mem)
        else new VerilatorWideOutputPortHandle(id, model, name, width, mem)
    }
  }
}

private[liftoff] class VerilatorWideInputPortHandle(
    val id: Int,
    val model: VerilatorSimModel,
    val name: String,
    val width: Int,
    val mem: MemorySegment
) extends InputPortHandle
    with VerilatorPortHandle {

  val words = (width + 31) / 32
  val seg = mem.reinterpret(words * 4)
  val bytes = new Array[Byte](words * 4)

  def get(): BigInt = {
    for (i <- 0 until 4 * words) {
      bytes(i) = seg.get(ValueLayout.JAVA_BYTE, (4 * words) - i)
    }
    BigInt(bytes)
  }

  def set(value: BigInt): Unit = {
    val bytes = value.toByteArray
    for (i <- 0 until 4 * words) {
      val byte = if (i < bytes.length) bytes(bytes.length - 1 - i) else 0.toByte
      seg.set(ValueLayout.JAVA_BYTE, (4 * words) - i, byte)
    }
  }
}

private[liftoff] class VerilatorWideOutputPortHandle(
    val id: Int,
    val model: VerilatorSimModel,
    val name: String,
    val width: Int,
    val mem: MemorySegment
) extends OutputPortHandle
    with VerilatorPortHandle {

  val words = (width + 31) / 32
  val seg = mem.reinterpret(words * 4)
  val bytes = new Array[Byte](words * 4)

  def get(): BigInt = {
    for (i <- 0 until 4 * words) {
      bytes(i) = seg.get(ValueLayout.JAVA_BYTE, (4 * words) - i)
    }
    BigInt(bytes)
  }

}

private[liftoff] class VerilatorU8InputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends InputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(1)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_BYTE, 0, value.toByte)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_BYTE, 0)) & mask
  }
}

private[liftoff] class VerilatorU16InputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends InputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(2)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_SHORT, 0, value.toShort)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_SHORT, 0)) & mask
  }
}

private[liftoff] class VerilatorU32InputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends InputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(4)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_INT, 0, value.toInt)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_INT, 0)) & mask
  }
}

private[liftoff] class VerilatorU64InputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends InputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(8)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_LONG, 0, value.toLong)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_LONG, 0)) & mask
  }
}

private[liftoff] class VerilatorU8OutputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends OutputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(1)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_BYTE, 0)) & mask
  }

}

private[liftoff] class VerilatorU16OutputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends OutputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(2)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_SHORT, 0)) & mask
  }

}

private[liftoff] class VerilatorU32OutputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends OutputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(4)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_INT, 0)) & mask
  }

}

private[liftoff] class VerilatorU64OutputPortHandle(
    val model: VerilatorSimModel,
    val name: String,
    val id: Int,
    val width: Int,
    val mem: MemorySegment
) extends OutputPortHandle
    with VerilatorPortHandle {

  val seg = mem.reinterpret(8)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_LONG, 0)) & mask
  }

}
