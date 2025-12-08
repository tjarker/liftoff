package liftoff.simulation.verilator


import liftoff.simulation._
import liftoff.misc.SharedObject
import liftoff.misc.WorkingDirectory
import liftoff.misc.WordArrayOps
import liftoff.misc.BigIntOps
import liftoff.simulation.Time._

import java.io.File
import liftoff.misc.Reporting

object VerilatorSimModelFactory {

  def create(
      topName: String,
      dir: WorkingDirectory,
      sources: Seq[File],
      verilatorOptions: Seq[Verilator.Argument],
      cOptions: Seq[String]
  )= {

    val verilatorDir = dir.addSubDir(dir / "verilator")

    val verilateRecipe = Verilator.createRecipe(
      verilatorDir,
      topName,
      Seq(
        Verilator.Arguments.CC,
        Verilator.Arguments.Build,
        Verilator.Arguments.TraceFst,
        Verilator.Arguments.CFlags("-fPIC -fpermissive")
      ) ++ verilatorOptions,
      sources
    )

    val artifacts = verilateRecipe.invoke()

    val portDescriptors = PortCollector.collectPorts(verilatorDir / s"V${topName}.h")

    //Reporting.debug(None, "Verilator", s"Collected ports:\n - ${portDescriptors.mkString("\n - ")}")

    val harnessFile = VerilatorModelHarness.writeHarness(
      dir,
      topName,
      portDescriptors
    )

    val harnessCompileRecipe = verilatorDir.addRecipe(
      Seq(dir / s"${topName}_harness.o"),
      Seq(harnessFile),
      Seq(
        "g++",
        "-I.") ++
        Verilator.getIncludeDir().get.map(p => s"-I$p").dropRight(1) ++ Seq(
        "-fPIC",
        "-fpermissive",
        "-c",
        "-o",
        (dir / s"${topName}_harness.o").getAbsolutePath(),
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
      libname = s"lib${topName}",
      dir,
      sources = artifacts :+ compiledHarness,
      options = Seq(
          "-lz",
        ) ++ extraCOptions ++ cOptions
    )

    val sharedObject = sharedObjectRecipe.invoke()

    new VerilatorSimModelFactory(
      topName,
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
  val ports: Seq[VerilatorPortDescriptor],
  val libFile: SharedObject
) {

  val lib = libFile.load()

  val createContextHandle =
    lib.getFunction(VerilatorModelHarness.createContextFunName(name))
  val deleteContextHandle =
    lib.getFunction(VerilatorModelHarness.deleteContextFunName(name))
  val evalHandle =
    lib.getFunction(VerilatorModelHarness.evalFunName(name))
  val tickHandle =
    lib.getFunction(VerilatorModelHarness.tickFunName(name))
  val setHandle =
    lib.getFunction(VerilatorModelHarness.setFunName(name))
  val getHandle =
    lib.getFunction(VerilatorModelHarness.getFunName(name))
  val setWideHandle =
    lib.getFunction(VerilatorModelHarness.setWideFunName(name))
  val getWideHandle =
    lib.getFunction(VerilatorModelHarness.getWideFunName(name))
  val quackHandle =
    lib.getFunction(VerilatorModelHarness.quackFunName(name))

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

  // Create a context for the model
  val contextPtr = factory.createContextHandle.invokePointer(
    Array(
      (dir / "wave.fst").getAbsolutePath(),
      "1ns",
      Array.empty[String],
      0
    )
  )

  override def inputs: Seq[InputPortHandle] = ports.collect {
    case i: InputPortHandle => i
  }

  override def outputs: Seq[OutputPortHandle] = ports.collect {
    case o: OutputPortHandle => o
  }

  override def getInputPortHandle(portName: String): InputPortHandle = ports.find { p =>
    p.name == portName && p.isInstanceOf[InputPortHandle]
  } match {
    case Some(port: InputPortHandle) => port
    case _ => throw new RuntimeException(
      s"Input port '$portName' not found in model '$name'."
    )
  }

  override def getOutputPortHandle(portName: String): OutputPortHandle = ports.find { p =>
    p.name == portName && p.isInstanceOf[OutputPortHandle]
  } match {
    case Some(port: OutputPortHandle) => port
    case _ => throw new RuntimeException(
      s"Output port '$portName' not found in model '$name'."
    )
  }

  override def evaluate(): Unit = {
    factory.evalHandle.invokeVoid(Array(contextPtr))
  }

  override def tick(delta: Time.RelativeTime): Unit = {
    factory.tickHandle.invokeVoid(Array(contextPtr, delta.valueFs))
  }

  override def cleanup(): Unit = {
    factory.deleteContextHandle.invoke(Array(contextPtr))
  }


  def get(handle: VerilatorPortHandle): BigInt = {
    handle match {
      case VerilatorPortHandle(name, id, width) if width <= 64 =>
        val result = factory.getHandle.invokeLong(
          Array(contextPtr, id)
        )
        BigInt(result)
      case VerilatorPortHandle(name, id, width) if width > 64 =>
        val valueArray = Array.ofDim[Int]((width + 31) / 32)
        factory.getWideHandle.invokeVoid(
          Array(contextPtr, id, valueArray)
        )
        valueArray.toBigInt
    }
  }

  def set(handle: VerilatorInputPortHandle, value: BigInt): Unit = {
    handle match {
      case VerilatorInputPortHandle(model, name, id, width) if width <= 64 =>
        factory.setHandle.invokeVoid(
          Array(contextPtr, id, value.toLong)
        )
      case VerilatorInputPortHandle(model, name, id, width) if width > 64 =>
        val valueArray = value.toWordArray
        factory.setWideHandle.invokeVoid(
          Array(contextPtr, id, valueArray)
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