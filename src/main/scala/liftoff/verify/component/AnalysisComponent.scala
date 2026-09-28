package liftoff.verify.component

import liftoff.verify.Component
import liftoff.verify.SimPhase
import liftoff.verify.ReportPhase
import liftoff.verify.Port

/** Receives the transactions of the monitors it subscribes to. */
abstract class AnalysisComponent[T] extends Component with SimPhase with ReportPhase {

  val port = Port.receiver[T]

  protected def next(): T = {
    port.receive()
  }

  protected def foreachTx(f: T => Unit): Unit = {
    while (true) {
      f(next())
    }
  }

  def subscribe[T1 <: T](m: Monitors[T]): Unit = {
    m.addSubscriber(this)
  }

}

/** An analysis component that checks transactions. */
abstract class Scoreboard[T] extends AnalysisComponent[T] {}
