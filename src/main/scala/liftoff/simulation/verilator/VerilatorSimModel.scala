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

object VerilatorSimModelFactory {

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
      verilatorOptions: Seq[Verilator.Argument],
      cOptions: Seq[String]
  )= {

    if (buildDirs.contains(dir)) {
      throw new Exception(s"Working dir for $topName (${dir.dir.getAbsolutePath()}) has already been used in this run.")
    }
    buildDirs += dir

    val verilatorDir = dir.addSubDir(dir / "verilator")

    val verilateRecipe = Verilator.createRecipe(
      verilatorDir,
      topName,
      Seq(
        Verilator.Arguments.CC,
        Verilator.Arguments.Build,
        Verilator.Arguments.TraceFst,
        Verilator.Arguments.OptimizationLevel("3"),
        Verilator.Arguments.CFlags("-fPIC -fpermissive -O3")
      ) ++ verilatorOptions,
      sources
    )

    val artifacts = verilateRecipe.invoke()

    val portDescriptors = PortCollector.collectPorts(verilatorDir / s"V${topName}.h")

    //Reporting.debug(None, "Verilator", s"Collected ports:\n - ${portDescriptors.mkString("\n - ")}")

    val functionPrefix = uniqueName(topName)

    val harnessFile = VerilatorModelHarness.writeHarness(
      dir,
      topName,
      functionPrefix,
      portDescriptors
    )

    val harnessCompileRecipe = verilatorDir.addRecipe(
      Seq(dir / s"${functionPrefix}_harness.o"),
      Seq(harnessFile),
      Seq(
        "g++",
        "-I.") ++
        Verilator.getIncludeDir().get.map(p => s"-I$p").dropRight(1) ++ Seq(
        "-fPIC",
        "-O3",
        "-fpermissive",
        "-c",
        "-o",
        (dir / s"${functionPrefix}_harness.o").getAbsolutePath(),
        harnessFile.getAbsolutePath()
      ),
      _.head
    )

    val compiledHarness = harnessCompileRecipe.invoke()


    val extraCOptions = 
      if (System.getProperty("os.name").toLowerCase.contains("windows")) Seq()
      else if (System.getProperty("os.name").toLowerCase.contains("mac")) Seq()
      else Seq("-pthread", "-lpthread", "-latomic")

    val sharedObjectRecipe = SharedObject.createRecipe(
      libname = s"lib${functionPrefix}",
      dir,
      sources = artifacts :+ compiledHarness,
      options = Seq(
          "-lz",
        ) ++ extraCOptions ++ cOptions
    )

    val sharedObject = sharedObjectRecipe.invoke()

    new VerilatorSimModelFactory(
      topName,
      functionPrefix,
      portDescriptors,
      sharedObject
    )
  }

}


/*

harness:
	g++ -I. -I/usr/share/verilator/include -I/usr/share/verilator/include/vltstd -fPIC -fpermissive -c -o new_ALU_harness.o ../ALU_harness.cpp

other:
	g++ -shared -fPIC ./new_ALU_harness.o verilated.o verilated_fst_c.o verilated_threads.o VALU__ALL.a   -lz  -pthread -lpthread -latomic   -o libtest.so

*/

class VerilatorSimModelFactory(
  val name: String,
  val functionPrefix: String,
  val ports: Seq[VerilatorPortDescriptor],
  val libFile: SharedObject
) {

  val lib = libFile.load()

  import ValueLayout._

  val createContextHandle = lib.functionHandle(VerilatorModelHarness.createContextFunName(functionPrefix), FunctionDescriptor.of(
      ADDRESS, // returns a pointer to the context
      ADDRESS, // wave file path
      ADDRESS, // time unit
      ADDRESS, // args
      JAVA_INT // num args
    ))
  val deleteContextHandle = lib.functionHandle(VerilatorModelHarness.deleteContextFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS))
  val evalHandle =
    lib.functionHandle(VerilatorModelHarness.evalFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS))
  val tickHandle =
    lib.functionHandle(VerilatorModelHarness.tickFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG))
  val quackHandle =
    lib.functionHandle(VerilatorModelHarness.quackFunName(functionPrefix), FunctionDescriptor.ofVoid())
  val getPointerHandle =
    lib.functionHandle(VerilatorModelHarness.getPointerFunName(functionPrefix), FunctionDescriptor.of(ADDRESS, ADDRESS, JAVA_LONG))

  def createModel(dir: WorkingDirectory): VerilatorSimModel = {
    new VerilatorSimModel(name, ports, this, dir)
  }

}


class VerilatorSimModel(
  val name: String,
  val portDescriptors: Seq[VerilatorPortDescriptor],
  val factory: VerilatorSimModelFactory,
  val dir: WorkingDirectory
) extends SimModel {

  
  val waveFile: File = dir / "wave.fst"

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
  

  override def inputs: Seq[InputPortHandle] = ports.collect {
    case i: InputPortHandle => i
  }

  override def outputs: Seq[OutputPortHandle] = ports.collect {
    case o: OutputPortHandle => o
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

trait VerilatorPortDescriptor {
  def name: String
  def id: Int
  def width: Int
}
object VerilatorPortDescriptor {
  def unapply(desc: VerilatorPortDescriptor): Option[(String, Int, Int)] = {
    Some((desc.name, desc.id, desc.width))
  }
}
case class VerilatorInputDescriptor(
  name: String,
  id: Int,
  width: Int
) extends VerilatorPortDescriptor

case class VerilatorOutputDescriptor(
  name: String,
  id: Int,
  width: Int
) extends VerilatorPortDescriptor

trait VerilatorPortHandle extends PortHandle {
  def id : Int
  def model: VerilatorSimModel
  val address: MemorySegment = model.factory.getPointerHandle.invokeExact(model.contextPtr, id.toLong)
  def get(): BigInt
  val mask = (BigInt(1) << width) - 1
}

object VerilatorPortHandle {
  def unapply(handle: VerilatorPortHandle): Option[(String, Int, Int)] = {
    Some((handle.model.name, handle.id, handle match {
      case i: InputPortHandle => i.width
      case o: OutputPortHandle => o.width
      case _ => 0
    }))
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



class VerilatorWideInputPortHandle(
  val id: Int,
  val model: VerilatorSimModel,
  val name: String,
  val width: Int,
  val mem: MemorySegment
) extends InputPortHandle with VerilatorPortHandle {

  val words = (width + 31) / 32
  val seg = mem.reinterpret(words * 4)
  val bytes = new Array[Byte](words * 4)

  def get(): BigInt = {
    for (i <- 0 until 4*words) {
      bytes(i) = seg.get(ValueLayout.JAVA_BYTE, (4*words) - i)
    }
    BigInt(bytes)
  }

  def set(value: BigInt): Unit = {
    val bytes = value.toByteArray
    for (i <- 0 until 4*words) {
      val byte = if (i < bytes.length) bytes(bytes.length - 1 - i) else 0.toByte
      seg.set(ValueLayout.JAVA_BYTE, (4*words) - i, byte)
    }
  }
}

class VerilatorWideOutputPortHandle(
  val id: Int,
  val model: VerilatorSimModel,
  val name: String,
  val width: Int,
  val mem: MemorySegment
) extends OutputPortHandle with VerilatorPortHandle {

  val words = (width + 31) / 32
  val seg = mem.reinterpret(words * 4)
  val bytes = new Array[Byte](words * 4)

  def get(): BigInt = {
    for (i <- 0 until 4*words) {
      bytes(i) = seg.get(ValueLayout.JAVA_BYTE, (4*words) - i)
    }
    BigInt(bytes)
  }

}

class VerilatorU8InputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends InputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(1)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_BYTE, 0, value.toByte)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_BYTE, 0)) & mask
  }
}

class VerilatorU16InputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends InputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(2)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_SHORT, 0, value.toShort)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_SHORT, 0)) & mask
  }
}

class VerilatorU32InputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends InputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(4)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_INT, 0, value.toInt)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_INT, 0)) & mask
  }
}

class VerilatorU64InputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends InputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(8)

  def set(value: BigInt): Unit = {
    seg.set(ValueLayout.JAVA_LONG, 0, value.toLong)
  }
  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_LONG, 0)) & mask
  }
}

class VerilatorU8OutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends OutputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(1)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_BYTE, 0)) & mask
  }
  
}

class VerilatorU16OutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends OutputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(2)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_SHORT, 0)) & mask
  }
  
}

class VerilatorU32OutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends OutputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(4)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_INT, 0)) & mask
  }
  
}


class VerilatorU64OutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int,
  val mem: MemorySegment
) extends OutputPortHandle with VerilatorPortHandle {

  val seg = mem.reinterpret(8)

  def get(): BigInt = {
    BigInt(seg.get(ValueLayout.JAVA_LONG, 0)) & mask
  }
  
}