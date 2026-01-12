package liftoff.simulation

import liftoff.simulation.control.SimController
import liftoff.simulation.control.CtrlClockHandle
import liftoff.simulation.control.CtrlPortHandle
import liftoff.simulation.control.CtrlInputHandle
import liftoff.simulation.control.CtrlOutHandle
import liftoff.simulation.Time
import liftoff.simulation.Time.AbsoluteTime
import liftoff.simulation.Time.RelativeTime
import liftoff.simulation.StepUntilResult
import liftoff.simulation.control.SimControllerResponse
import liftoff.simulation.control.SimControllerYield
import liftoff.coroutine.CoroutineContext
import liftoff.simulation.task.Task
import liftoff.simulation.control.TickFor

class SimTime(t: Long) extends AbsoluteTime(t) {

  def tick(delta: Time): Unit = {
    SimController.current.suspendWith(TickFor(delta.relative))
  }

}

object Sim {

  object Model {
    def getInputPortHandle(portName: String): Option[InputPortHandle] = {
      SimController.current.getInputPortHandle(portName)
    }
    def getOutputPortHandle(portName: String): Option[OutputPortHandle] = {
      SimController.current.getOutputPortHandle(portName)
    }
    def addClockDomain(clockPortName: String, period: Time, ports: Seq[PortHandle]): ClockPortHandle = {
      require(ports.forall(_.isInstanceOf[CtrlPortHandle]), s"Can't add clock domain with non-CtrlPortHandle ports: ${ports.filterNot(_.isInstanceOf[CtrlPortHandle])}")
      SimController.current.addClockDomain(clockPortName, period, ports.asInstanceOf[Seq[CtrlPortHandle]])
    }
    def getCycles(c: ClockPortHandle): Int = {
      require(c.isInstanceOf[CtrlClockHandle], s"Can't get cycles of non-CtrlClockHandle: $c")
      SimController.current.getCycle(c.asInstanceOf[CtrlClockHandle])
    }
    def addCombinationalDependency(output: OutputPortHandle, inputs: Seq[InputPortHandle]): Unit = {
      require(output.isInstanceOf[CtrlOutHandle], s"Can't add combinational dependency to non-CtrlOutHandle: $output")
      require(inputs.forall(_.isInstanceOf[CtrlInputHandle]), s"Can't add combinational dependency from non-CtrlInputHandle: ${inputs.filterNot(_.isInstanceOf[CtrlInputHandle])}")
      SimController.current.addCombinationDependency(output.asInstanceOf[CtrlOutHandle], inputs.asInstanceOf[Seq[CtrlInputHandle]])
    }
  }
  def time: SimTime = SimController.current.currentTime

  object Scheduler {
    def suspendTask(v: SimControllerYield): Unit = {
      SimController.current.suspendWith(v)
    }
    def suspendTask(): Unit = {
      SimController.current.suspend()
    }
    def addTask[T](name: String, order: Int, ctx: Option[CoroutineContext] = None)(block: => T): Task[T] = {
      SimController.current.addTask[T](name, order, ctx)(block)
    }
    def scheduleTaskAt(time: AbsoluteTime, task: Task[_]): Unit = {
      SimController.current.scheduleTaskAt(time, task)
    }
    def scheduleTaskNow(task: Task[_]): Unit = {
      SimController.current.scheduleTaskNow(task)
    }
    def purgeTask(task: Task[_]): Unit = {
      SimController.current.purgeTask(task)
    }
  }

}