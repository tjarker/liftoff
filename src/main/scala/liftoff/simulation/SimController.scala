package liftoff.simulation

import liftoff.simulation.Time._
import liftoff.coroutine.Coroutine
import liftoff.coroutine.{Finished, YieldedWith, Failed}
import scala.util.DynamicVariable
import liftoff.misc.Reporting
import liftoff.coroutine.Yielded
import chisel3.Output
import liftoff.simulation.task.Task

trait SimControllerYield
case class Step(clockPort: InputPortHandle, cycles: Int) extends SimControllerYield
//case class StepUntil(clockPort: InputPortHandle, port: OutputPortHandle, value: BigInt, maxCycles: Int) extends SimControllerYield
case class TickFor(duration: RelativeTime) extends SimControllerYield
case class TickUntil(time: AbsoluteTime) extends SimControllerYield

object SimController {

  private val dynamicVariable = new scala.util.DynamicVariable[SimController](null)
  def current: SimController = dynamicVariable.value

  private val dynId = new DynamicVariable[Int](0)
  def currentId: Int = dynId.value

  var id = 0

  def runWith[T](ctrl: SimController)(block: => T): T = {
    dynamicVariable.withValue(ctrl) {
      id += 1
      dynId.withValue(id) {
        block
      }
    }
  }
  def set(ctrl: SimController)= {
    dynamicVariable.value = ctrl
  }
}

object Sim {

  object Model {
    def set(port: InputPortHandle, value: BigInt): Unit = {
      SimController.current.set(port, value)
    }
    def get(port: PortHandle, isSigned: Boolean = false): BigInt = {
      SimController.current.get(port, isSigned)
    }
    def getInputPortHandle(portName: String): Option[InputPortHandle] = {
      SimController.current.getInputPortHandle(portName)
    }
    def getOutputPortHandle(portName: String): Option[OutputPortHandle] = {
      SimController.current.getOutputPortHandle(portName)
    }
  }
  def time: AbsoluteTime = SimController.current.currentTime

  object Scheduler {
    def suspendTask(v: SimControllerYield): Unit = {
      SimController.current.suspendWith(v)
    }
    def suspendTask(): Unit = {
      SimController.current.suspend()
    }
    def addTask[T](name: String, order: Int)(block: => T): Task[T] = {
      SimController.current.addTask[T](name, order)(block)
    }
    def scheduleTaskAt(time: AbsoluteTime, task: Task[_]): Unit = {
      SimController.current.scheduleTaskAt(time, task)
    }
    def scheduleTaskNow(task: Task[_]): Unit = {
      SimController.current.scheduleTaskNow(task)
    }
  }

}

class SimControllerInputHandle(p: InputPortHandle, ctrl: SimController) extends InputPortHandle {
  
  def width: Int = p.width
  def name: String = p.name

  def get(): BigInt = {
    ctrl.get(p, isSigned = false)
  }
  def set(value: BigInt): Unit = {
    ctrl.set(p, value)
  }
}

class SimControllerOutputHandle(p: OutputPortHandle, ctrl: SimController) extends OutputPortHandle {
  
  def width: Int = p.width
  def name: String = p.name

  def get(): BigInt = {
    ctrl.get(p, isSigned = false)
  }
}

class SimController(simModel: SimModel) {

  val taskScope = Coroutine.createScope()

  val eventQueue: EventQueue = new EventQueue
  var currentTime: Time.AbsoluteTime = 0.fs.absolute

  val outputCache = collection.mutable.Map.empty[OutputPortHandle, BigInt]
  val inputCache = collection.mutable.Map.empty[InputPortHandle, BigInt]
  val inputDirty = collection.mutable.Set.empty[InputPortHandle] // TODO: update this on set() and get()

  val inputHandles = simModel.inputs.map(p => p.name -> new SimControllerInputHandle(p, this)).toMap
  val outputHandles = simModel.outputs.map(p => p.name -> new SimControllerOutputHandle(p, this)).toMap

  def ports: Seq[PortHandle] = (inputHandles.values ++ outputHandles.values).toSeq

  val clockPeriods = collection.mutable.Map.empty[InputPortHandle, Time]
  val portToClock = collection.mutable.Map.empty[PortHandle, InputPortHandle]

  def run(): Unit = {
    
    while (eventQueue.containsActiveTasks) {
     
      val event = eventQueue.pop().get

      Reporting.debug(Some(currentTime), "SimController", s"Handling event: ${event}")
      
      val delta = event.time - currentTime
      if (delta > 0.fs) {
        simModel.tick(delta.relative)
        currentTime = event.time
      }

      handleEvent(event)

      Reporting.debug(Some(currentTime), "SimController", s"Event queue:\n - ${eventQueue.queue.mkString("\n - ")}")
    }
    
  }

  def handleEvent(event: Event): Unit = {
    event match {
      case Event.ClockEdge(_, clockPort, period, rising) =>
        if (rising) {
          // clear output cache on clock edges
          outputCache.clear()
          inputDirty.clear()
          clockPort.set(1)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (period / 2)).absolute, clockPort, period, false))
        } else {
          clockPort.set(0)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (period / 2)).absolute, clockPort, period, true))
        }
      case Event.RunTask(_, task, _) =>
        handleTask(task)
    }
  }

  def addClockDomain(clock: InputPortHandle, period: Time, ports: Seq[PortHandle]): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Adding clock: ${clock.name} with period ${period}")
    clockPeriods(clock) = period
    ports.foreach(p => portToClock(p) = clock)
    eventQueue.enqueue(Event.ClockEdge(currentTime.absolute, clock, period, false))
  }

  def handleTask(t: Task[_]) = {
    t.runStep() match {


          case Finished(result) => // do nothing
            Reporting.debug(Some(currentTime), "SimController", s"Task $t finished")


          case YieldedWith(Step(clockPort, cycles)) =>
            Reporting.debug(Some(currentTime), "SimController", s"Stepping task ${t.name} for ${cycles} cycles on clock ${clockPort}")


            val nextFallingEdge = eventQueue.nextFallingEdge(clockPort).getOrElse {
              Reporting.error(Some(currentTime), "SimController", s"No falling edge scheduled for clock ${clockPort.name} when trying to step task ${t.name}")
              0.fs.absolute
            }
            val period = clockPeriods.getOrElse(clockPort, {
              Reporting.error(Some(currentTime), "SimController", s"No period recorded for clock ${clockPort.name} when trying to step task ${t.name}")
              throw new Exception("No clock period")
            })
            val nextTime = if (nextFallingEdge == currentTime) nextFallingEdge + (period * cycles) else nextFallingEdge + (period * (cycles - 1))
            Reporting.debug(Some(currentTime), "SimController", s"Scheduling task ${t.name} at time ${nextTime} after ${cycles}x${period}")
            eventQueue.enqueue(Event.RunTask(nextTime.absolute, t, t.order))

          case YieldedWith(TickFor(duration)) =>
            Reporting.debug(Some(currentTime), "SimController", s"Ticking task ${t.name} for duration ${duration}")
            val nextTime = currentTime + duration
            eventQueue.enqueue(Event.RunTask(nextTime.absolute, t, t.order))

          case YieldedWith(TickUntil(time)) =>
            Reporting.debug(Some(currentTime), "SimController", s"Ticking task ${t.name} until time ${time}")
            eventQueue.enqueue(Event.RunTask(time.absolute, t, t.order))
          
          case Yielded => // do nothing, will be resumed manually


          case Failed(e) =>
            Reporting.error(Some(currentTime), "SimController", s"Active task ${t.name} failed with exception: ${e}")
            throw e
        }
  }

  
  def getInputPortHandle(portName: String): Option[InputPortHandle] = inputHandles.get(portName)

  def getOutputPortHandle(portName: String): Option[OutputPortHandle] = outputHandles.get(portName)
  
  def get(port: PortHandle, isSigned: Boolean): BigInt = {
    Reporting.debug(Some(currentTime), "SimController", s"Getting value of port: ${port.name}")
    val nextSamplingTime = portToClock.get(port) match {
      case Some(clockPort) =>
        eventQueue.nextFallingEdge(clockPort).getOrElse {
          Reporting.error(Some(currentTime), "SimController", s"No rising edge scheduled for clock ${clockPort.name} when trying to sample port ${port.name}")
          throw new Exception("No clock period")
        }
      case None => currentTime
    }
    if (currentTime != nextSamplingTime) {
      Reporting.debug(Some(currentTime), "SimController", s"Advancing time from ${currentTime} to ${nextSamplingTime} to sample port ${port.name}")
      taskScope.suspend(Some(TickUntil(nextSamplingTime.absolute)))
      Reporting.debug(Some(currentTime), "SimController", s"Resumed for sampling port ${port.name} at time ${currentTime}")
    }
    port match {
      case iph: InputPortHandle =>
        if (inputDirty.contains(iph)) Reporting.warn(Some(currentTime), "SimController", s"Reading dirty input port ${iph.name}")
        inputDirty.add(iph)
        inputCache.getOrElseUpdate(iph, iph.get())
      case oph: OutputPortHandle =>
        outputCache.getOrElseUpdate(oph, oph.get())
    }
  }

  def set(port: InputPortHandle, value: BigInt): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Setting value of port: ${port.name} to ${value}")
    val nextDriveTime = portToClock.get(port) match {
      case Some(clockPort) =>
        eventQueue.nextFallingEdge(clockPort).getOrElse {
          Reporting.error(Some(currentTime), "SimController", s"No rising edge scheduled for clock ${clockPort.name} when trying to drive port ${port.name}")
          throw new Exception("No clock period")
        }
      case None => currentTime
    }
    if (currentTime != nextDriveTime) {
      Reporting.debug(Some(currentTime), "SimController", s"Advancing time from ${currentTime} to ${nextDriveTime} to drive port ${port.name}")
      taskScope.suspend(Some(TickUntil(nextDriveTime.absolute)))
      Reporting.debug(Some(currentTime), "SimController", s"Resumed for driving port ${port.name} at time ${currentTime}")
    }
    if (!clockPeriods.exists { case (clockPort, _) => clockPort.name == port.name } && inputDirty.contains(port)) {
      Reporting.error(Some(currentTime), "SimController", s"Overwriting input port ${port.name} after it has been read")
    }
    inputDirty.add(port)
    inputCache(port) = value
    port.set(value)
  }

  def addTask[T](name: String, order: Int)(block: => T): Task[T] = {
    Reporting.debug(Some(currentTime), "SimController", s"Adding task: ${name} with order ${order}")
    var task: Task[T] = null
    task = new Task[T](name, taskScope, order, {
      Task.withValue[T](task) {
        block
      }
    })
    eventQueue.enqueue(Event.RunTask(currentTime, task, order))
    task
  }

  def scheduleTaskAt(time: AbsoluteTime, task: Task[_]): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Scheduling task ${task.name} at time ${time}")
    eventQueue.enqueue(Event.RunTask(time, task, task.order))
  }

  def scheduleTaskNow(task: Task[_]): Unit = scheduleTaskAt(currentTime, task)

  def suspendWith(v: SimControllerYield): Unit = {
    taskScope.suspend(Some(v))
  }

  def suspend(): Unit = {
    taskScope.suspend[Unit, SimControllerYield](None)
  }

}
