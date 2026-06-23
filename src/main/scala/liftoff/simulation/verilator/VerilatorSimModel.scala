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
  val setHandle =
    lib.functionHandle(VerilatorModelHarness.setFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG, JAVA_LONG))
  val getHandle =
    lib.functionHandle(VerilatorModelHarness.getFunName(functionPrefix), FunctionDescriptor.of(JAVA_LONG, ADDRESS, JAVA_LONG))
  val setWideHandle =
    lib.functionHandle(VerilatorModelHarness.setWideFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG, ADDRESS))
  val getWideHandle =
    lib.functionHandle(VerilatorModelHarness.getWideFunName(functionPrefix), FunctionDescriptor.ofVoid(ADDRESS, JAVA_LONG, ADDRESS))
  val quackHandle =
    lib.functionHandle(VerilatorModelHarness.quackFunName(functionPrefix), FunctionDescriptor.ofVoid())

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

  val ports: Seq[VerilatorPortHandle] = portDescriptors.map {
    case VerilatorInputDescriptor(n, id, w) =>
      VerilatorInputPortHandle(this, n, id, w)
    case VerilatorOutputDescriptor(n, id, w) =>
      VerilatorOutputPortHandle(this, n, id, w)
  }
  
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

  override def cleanup(): Unit = {
    factory.deleteContextHandle.invokeExact(contextPtr)
  }


  def get(handle: VerilatorPortHandle): BigInt = {
    val value = handle match {
      case VerilatorPortHandle(name, id, width) if width <= 64 =>
        val result: Long = factory.getHandle.invokeExact(contextPtr, id.toLong)
        BigInt(result)
      case VerilatorPortHandle(name, id, width) if width > 64 =>
        val valueArray = Array.ofDim[Int]((width + 31) / 32)
        factory.getWideHandle.invokeExact(contextPtr, id.toLong, valueArray)
        valueArray.toBigInt
    }
    //Reporting.debug(None, "VerilatorSimModel", s"Getting value of port ${handle.name} with id ${handle.id}: ${value}")
    value
  }

  def set(handle: VerilatorInputPortHandle, value: BigInt): Unit = {
    //Reporting.debug(None, "VerilatorSimModel", s"Setting value of port ${handle.name} with id ${handle.id} to ${value}")
    val mask = (BigInt(1) << handle.width) - 1
    val maskedValue = value & mask
    handle match {
      case VerilatorInputPortHandle(model, name, id, width) if width <= 64 =>
        factory.setHandle.invokeExact(contextPtr, id.toLong, maskedValue.toLong)
      case VerilatorInputPortHandle(model, name, id, width) if width > 64 =>
        val valueArray = maskedValue.toWordArray
        factory.setWideHandle.invokeExact(
          contextPtr, id.toLong, valueArray
        )
      case _ =>
        throw new RuntimeException(
          s"Can't set port handle: $handle"
        )
    }
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
  def get(): BigInt = model.get(this)
}

object VerilatorPortHandle {
  def unapply(handle: VerilatorPortHandle): Option[(String, Int, Int)] = {
    Some((handle.name, handle.id, handle.width))
  }
}

case class VerilatorInputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int
) extends InputPortHandle with VerilatorPortHandle {
   def set(value: BigInt): Unit = model.set(this, value)
}

case class VerilatorOutputPortHandle(
  val model: VerilatorSimModel,
  val name: String,
  val id: Int,
  val width: Int
) extends OutputPortHandle with VerilatorPortHandle {
  
}