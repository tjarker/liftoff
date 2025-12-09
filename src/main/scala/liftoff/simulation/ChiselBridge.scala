package liftoff.simulation

import chisel3.Data
import chisel3.experimental.{SourceInfo, SourceLine}

trait AnySimulatedModule {

  def port(data: Data): Simulation.Port

  def willPeek(): Unit
  def willPoke(): Unit
  def willEvaluate(): Unit

  def controller: Controller

}



class DummySimulatedModule extends AnySimulatedModule {
  override def port(data: Data): Simulation.Port = new Simulation.Port(data.pathName)

  override def willPeek(): Unit = println(s"Will peek from module")

  override def willPoke(): Unit = println(s"Will poke from module")

  override def willEvaluate(): Unit = println(s"Will evaluate module")
  override def controller: Controller = new DummyController
}

trait Controller {
  def run(cycles: Int): Unit
}
class DummyController extends Controller {
  override def run(cycles: Int): Unit = println(s"Running simulation for $cycles cycles")
}

object AnySimulatedModule {
  def current: AnySimulatedModule = new DummySimulatedModule

}

object Simulation {
  class Port(name: String) {
    def get(isSigned: Boolean): Value = { println(s"Getting value of port $name"); new DummyValue(1, 0) }
    def set(value: BigInt): Unit = println(s"Setting port $name to value $value")
    def check(isSigned: Boolean)(checkFn: Value => Unit): Unit = println(s"Checking port $name")
    def tick(
      timestepsPerPhase: Int,
      maxCycles:         Int,
      inPhaseValue:      BigInt,
      outOfPhaseValue:   BigInt,
      sentinel:         Option[(Port, BigInt)]
    ): Unit = {
      println(s"Ticking port $name for up to $maxCycles cycles with $timestepsPerPhase timesteps per phase")
    }
  }

  trait Value {
    def bitCount: Int

    def asBigInt: BigInt
  }
  class DummyValue(bits: Int, value: BigInt) extends Value {
    override def bitCount: Int = bits

    override def asBigInt: BigInt = value
  }
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
