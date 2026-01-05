package liftoff.verify.component

import liftoff.verify.Component
import liftoff.verify.SimPhase
import liftoff.verify.Transaction
import liftoff.coroutine.BiGen
import liftoff.verify.Port
import liftoff.simulation.task.Task

import scala.collection.mutable

class DriveCompletion {
  private var finished = false

  private [verify] def markFinished(): Unit = {
    finished = true
    waiting.foreach { t =>
      liftoff.simulation.Sim.Scheduler.scheduleTaskNow(t)
    }
  }

  def isDone: Boolean = finished

  val waiting = mutable.Buffer[Task[_]]()

  def awaitDone(): Unit = {
    waiting.append(Task.current)
    liftoff.simulation.Sim.Scheduler.suspendTask()
  }
}

abstract class Driver[T <: Transaction, R <: Transaction] extends Component with SimPhase {

  val sequencePort = Port.receiver[(BiGen[R, T], DriveCompletion)]
  var currentGen = Option.empty[(BiGen[R, T], DriveCompletion)]
  

  val waitForGenList = mutable.Map.empty[BiGen[R, T], Task[_]]

  protected def next(): T = {
    if (currentGen.isEmpty) {
      currentGen = Some(sequencePort.receive())
    }

    currentGen match {
      case Some((gen, flag)) => {
        gen.next()
      }
      case None      => throw new Exception(s"Unreachable")
    }
  }

  def shouldRespond: Boolean = currentGen match {
    case Some((gen, _)) => gen.expectsFeedback
    case None    => false
  }

  def done(resp: R): Unit = {
    currentGen match {
      case Some((gen, flag)) => {
        gen.feedback(resp)
        if (!gen.hasNext) {
          flag.markFinished()
          currentGen = None
        }
      }
      case None      => throw new Exception(s"No current generator to respond to")
    }
  }

  def done(): Unit = {
    currentGen match {
      case Some((gen, flag)) => {
        if (!gen.hasNext) {
          flag.markFinished()
          currentGen = None
        }
      }
      case None      => throw new Exception(s"No current generator to respond to")
    }
  }


  // External interface
  def enqueue(gen: BiGen[R, T]): DriveCompletion = {
    val flag = new DriveCompletion()
    sequencePort.channel.send((gen, flag))
    flag
  }
  

}
