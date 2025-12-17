package liftoff.chiselbridge

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

  def emitSystemVerilogFile(m: => chisel3.RawModule, dir: WorkingDirectory) = {
    chisel3.emitVerilog(
      m,
      Array(
        "--target-dir",
        dir.toString,
      )
    )
  }

  trait Port {
    def get(isSigned: Boolean): Value
    def set(value: BigInt): Unit
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit
    def tick(
      timestepsPerPhase: Int,
      maxCycles:         Int,
      inPhaseValue:      BigInt,
      outOfPhaseValue:   BigInt,
      sentinel:         Option[(Port, BigInt)]
    ): Unit
    def handle: PortHandle
  }
  object Port {
    def fromHandle(handle: PortHandle): Port = handle match {
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
        ChiselBridge.Port.fromHandle(controller.getInputPortHandle(finalName) match {
        case Some(port) => port
        case None       => controller.getOutputPortHandle(finalName) match {
          case Some(port) => port
          case None       => throw new Exception(s"Could not find port handle for port: $finalName")
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
    def tick(
      timestepsPerPhase: Int,
      maxCycles:         Int,
      inPhaseValue:      BigInt,
      outOfPhaseValue:   BigInt,
      sentinel:         Option[(Port, BigInt)]
    ): Unit = SimController.current.suspendWith(Step(handle, 1))

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
    def tick(timestepsPerPhase: Int, maxCycles: Int, inPhaseValue: BigInt, outOfPhaseValue: BigInt, sentinel: Option[(Port, BigInt)]): Unit = 
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

  
