package liftoff.verify.decoupled

import chisel3.Data
import liftoff.simulation.Time

trait DecoupledTransfer[T <: Data] {
  import DecoupledTransfer._
  def data: T
  def cycleInitiated: Int
  def timeInitiated: Time
  def waitedCycles: Int
  def waitedTime: Time
  def isOk: Boolean = this match { case _: Ok[T] => true; case _ => false}
  def asOk: Ok[T] = this match {
    case ok: Ok[T] => ok
    case _ => throw new Exception("DecoupledTransfer is not Ok")
  }
  def timedOut: Boolean = this match { case _: Timeout[T] => true; case _ => false}

  override def toString(): String = this match {
    case Ok(data, cycleInitiated, cycleCompleted, timeInitiated, timeCompleted) =>
      s"Ok($data, start=$cycleInitiated, end=$cycleCompleted)"
    case Timeout(data, cycleInitiated, cycleTimedOut, timeInitiated, timeTimedOut) =>
      s"Timeout($data, start=$cycleInitiated, end=$cycleTimedOut)"
  }
}
object DecoupledTransfer {
  case class Ok[T <: Data](
    data: T, 
    cycleInitiated: Int,
    cycleCompleted: Int,
    timeInitiated: Time,
    timeCompleted: Time
  ) extends DecoupledTransfer[T] {
    def waitedCycles: Int = cycleCompleted - cycleInitiated
    def waitedTime: Time = timeCompleted - timeInitiated
  }
  case class Timeout[T <: Data](
    data: T,
    cycleInitiated: Int,
    cycleTimedOut: Int,
    timeInitiated: Time,
    timeTimedOut: Time,
  ) extends DecoupledTransfer[T] {
    def waitedCycles: Int = cycleTimedOut - cycleInitiated
    def waitedTime: Time = timeTimedOut - timeInitiated
  }
}