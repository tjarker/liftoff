package liftoff.verify.decoupled

import chisel3._
import chisel3.util._
import liftoff._

class DecoupledProducer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def send(data: T, timeout: Int = -1): DecoupledTransfer[T] = {
    val oldValue = port.bits.peek()
    port.bits.poke(data)
    port.valid.poke(1.B)
    val startCycle = clock.cycle
    val startTime = Sim.time

    val result = Task.withRegion(Region.Monitor) {

      clock.stepUntil(port.ready, 1.B, maxCycles = timeout) match {
        case StepUntilResult.Success(_) =>
          val acceptedCycle = clock.cycle
          val acceptedTime = Sim.time
          DecoupledTransfer.Ok(data, startCycle, acceptedCycle, startTime, acceptedTime)
        
        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          DecoupledTransfer.Timeout(data, startCycle, timedOutCycle, startTime, timedOutTime)
      }
    }.join()

    clock.step() // commit handshake
    port.valid.poke(0.B)
    port.bits.poke(oldValue)
    result
  }

  def sendAll(data: Seq[T], timeout: Int = -1): Seq[DecoupledTransfer[T]] = {
    data.map(send(_, timeout))
  }

}

class DecoupledConsumer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def receive(timeout: Int = -1): DecoupledTransfer[T] = {
    port.ready.poke(1.B)

    val result = Task.withRegion(Region.Monitor) {
      clock.stepUntil(port.valid, 1.B, maxCycles = timeout) match {

        case StepUntilResult.Success(_) =>
          val receivedCycle = clock.cycle
          val receivedTime = Sim.time
          val received = port.bits.peek()
          DecoupledTransfer.Ok(received, receivedCycle - 1, receivedCycle, receivedTime, receivedTime)
        
        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          DecoupledTransfer.Timeout(port.bits.peek(), timedOutCycle, timedOutCycle, timedOutTime, timedOutTime)
      }
    }.join()

    clock.step() // commit handshake
    port.ready.poke(0.B)
    result
  }

  def receiveAll(n: Int, timeout: Int = -1): Seq[DecoupledTransfer[T]] = {
    (0 until n).map(_ => receive(timeout))
  }

  def expect(expected: T, timeout: Int = -1): DecoupledTransfer[T] = {
    port.ready.poke(1.B)

    val result = Task.withRegion(Region.Monitor) {
      clock.stepUntil(port.valid, 1.B, maxCycles = timeout) match {

        case StepUntilResult.Success(_) =>
          val receivedCycle = clock.cycle
          val receivedTime = Sim.time
          val received = port.bits.peek()
          port.bits.expect(expected)
          DecoupledTransfer.Ok(received, receivedCycle - 1, receivedCycle, receivedTime, receivedTime)
        
        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          DecoupledTransfer.Timeout(port.bits.peek(), timedOutCycle, timedOutCycle, timedOutTime, timedOutTime)
      }
    }.join()

    clock.step() // commit handshake
    port.ready.poke(0.B)
    result
  }

  def expectAll(expected: Seq[T], timeout: Int = -1): Unit = {
    expected.foreach(expect(_, timeout))
  }
}

class DecoupledMonitor[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def observe(): DecoupledTransfer.Ok[T] = {
    clock.stepUntil(port.valid, 1.B)
    val startCycle = clock.cycle
    val startTime = Sim.time
    clock.stepUntil(port.ready, 1.B)
    val acceptedCycle = clock.cycle
    val acceptedTime = Sim.time
    val data = port.bits.peek()
    clock.step() // commit handshake
    DecoupledTransfer.Ok(data, startCycle, acceptedCycle, startTime, acceptedTime)
  }
}
