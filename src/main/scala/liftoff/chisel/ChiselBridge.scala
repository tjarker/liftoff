package liftoff.chisel

import chisel3.Data
import chisel3.experimental.{SourceInfo, SourceLine}
import liftoff.misc.WorkingDirectory
import circt.stage.ChiselStage

import liftoff.simulation._
import scala.runtime.BoxedUnit
import chisel3.Input


object ChiselBridge {

  def elaborate[M <: chisel3.RawModule](gen: => M): M = {
    val elaboratePhase = new chisel3.stage.phases.Elaborate
    val converter = new chisel3.stage.phases.Convert

    val genAnno = chisel3.stage.ChiselGeneratorAnnotation(() => gen)
    val elaborationAnnos: firrtl.AnnotationSeq =
      elaboratePhase.transform(Seq(genAnno))

    val dut: M = elaborationAnnos
      .collectFirst { case chisel3.stage.DesignAnnotation(d) => d }
      .get
      .asInstanceOf[M]

    dut
  }

  def emitSystemVerilogFile(name: String, m: => chisel3.RawModule, dir: WorkingDirectory): Seq[java.io.File] = {
    chisel3.emitVerilog(
      m,
      Array(
        "--target-dir",
        dir.toString,
      )
    )
    val mainFile = dir.addFile(s"${name}.v")
    // load firrtl_black_box_resource_files.f
    val blackBoxFiles = try {
      scala.io.Source
      .fromFile(dir / "firrtl_black_box_resource_files.f")
      .getLines()
      .map(new java.io.File(_))
      .toSeq
    } catch {
      case _: java.io.FileNotFoundException => Seq()
    } finally { Seq() }
    blackBoxFiles.foreach(f => dir.addFile(f.getName()))
    mainFile +: blackBoxFiles
  }

  trait Port {
    def get(isSigned: Boolean): Value
    def set(value: BigInt): Unit
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit
    def tick(cycles: Int): Unit
    def handle: PortHandle
  }
  object Port {
    def fromHandle(handle: PortHandle): Port = handle match {
      case ch: ClockPortHandle   => new ClockPort(ch)
      case ih: InputPortHandle   => new InputPort(ih)
      case oh: OutputPortHandle  => new OutputPort(oh)
    }
    def fromData(data: Data): Port = {
        val portName = data.toNamed.name
        val noDots = portName.replace('.', '_')

        // replace [(\d+)] with _$1
        val noBrackets = noDots.replaceAll("\\[(\\d+)\\]", "_$1")

        val finalName = noBrackets

        val controller = SimController.current
        ChiselBridge.Port.fromHandle(controller.getClockPortHandle(finalName) match { // first try to find clock
          case Some(clockPort) => clockPort
          case None => 
            controller.getInputPortHandle(finalName) match { // then input
            case Some(inputPort) => inputPort
            case None       => controller.getOutputPortHandle(finalName) match { // finally output
              case Some(outputPort) => outputPort
              case None       => throw new Exception(s"Could not find port handle for port: $finalName")
            }
          }
      })
    }
  }

  class InputPort(handle: InputPortHandle) extends Port {
    def set(value: BigInt): Unit = handle.set(value)
    def get(isSigned: Boolean): Value = {
      val v = handle.get()
      new Value(handle.width, v)
    }
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit = {
      val v = get(isSigned)
      checkFn(v)
    }
    def tick(cycles: Int): Unit = throw new Exception(s"Cannot tick input port handle: ${handle.name}")

    def handle: PortHandle = this.handle
  }

  class ClockPort(handle: ClockPortHandle) extends Port {

    def set(value: BigInt): Unit = 
      throw new Exception(s"Cannot set clock port handle: ${handle.name}")
    def get(isSigned: Boolean): Value = {
      throw new Exception(s"Cannot get clock port handle: ${handle.name}")
    }
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit = {
      throw new Exception(s"Cannot check clock port handle: ${handle.name}")
    }
    override def tick(cycles: Int): Unit = handle.step(cycles)
    def handle: PortHandle = this.handle
  }

  class OutputPort(handle: OutputPortHandle) extends Port {
    def get(isSigned: Boolean): Value = {
      val v = handle.get()
      new Value(handle.width, v)
    }
    def set(value: BigInt): Unit = {
      throw new Exception(s"Cannot set output port handle: ${handle.name}")
    }
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit = {
      val v = get(isSigned)
      checkFn(v)
    }
    def tick(cycles: Int): Unit = 
      throw new Exception(s"Cannot tick output port handle: ${handle.name}")

    def handle: PortHandle = this.handle
  }

  class Value(bits: Int, value: BigInt) {
    def bitCount: Int = bits

    def asBigInt: BigInt = value
  }


  object Message {
    def dramaticMessage(header: Option[String], body: String): String = {
      val headerLine = header.map(h => s"=== $h ===\n").getOrElse("")
      s"$headerLine$body"
    }
  }

  object ExceptionHelpers {
    def getErrorLineInFile(extraContext: Seq[String], sourceLine: SourceLine): Seq[String] = {
      Seq("this is a test")
    }
  }

}

  
