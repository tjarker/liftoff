package liftoff.verify.component

import liftoff.verify.Transaction
import liftoff.verify.Component
import liftoff.verify.SimPhase
import liftoff.verify.ReportPhase
import liftoff.verify.Port

abstract class AnalysisComponent[T <: Transaction] extends Component with SimPhase with ReportPhase {

  val port = Port.receiver[T]

  protected def next(): T = {
    port.receive()
  }

  protected def foreachTx(f: T => Unit): Unit = {
    while (true) {
      f(next())
    }
  }

}

abstract class Scoreboard[T <: Transaction] extends AnalysisComponent[T] {
  
}
