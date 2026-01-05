package liftoff.simulation

import liftoff.simulation.Time._
import liftoff.coroutine.Coroutine
import liftoff.coroutine.{Finished, YieldedWith, Failed}
import scala.util.DynamicVariable
import liftoff.misc.Reporting
import liftoff.coroutine.Yielded
import chisel3.Output
import liftoff.simulation.task.Task
import liftoff.coroutine.CoroutineContext
import liftoff.simulation.task.TaskScope

trait SimControllerYield
case class Step(clockPort: ClockPortHandle, cycles: Int) extends SimControllerYield
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
    def addClockDomain(clockPortName: String, period: Time, ports: Seq[PortHandle]): ClockPortHandle = {
      SimController.current.addClockDomain(clockPortName, period, ports)
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

class SimControllerClockHandle(p: InputPortHandle, ctrl: SimController, val period: Time) extends ClockPortHandle {
  
  def width: Int = p.width
  def name: String = p.name

  def get(): BigInt = {
    ctrl.get(p, isSigned = false)
  }
  def set(value: BigInt): Unit = {
    ctrl.set(p, value)
  }

  def doStep(n: Int): Unit = {
    ctrl.taskScope.suspend(Some(Step(this, n)))
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
  val clockHandles = collection.mutable.Map.empty[String, ClockPortHandle]

  def ports: Seq[PortHandle] = (inputHandles.values ++ outputHandles.values).toSeq

  val portToClock = collection.mutable.Map.empty[PortHandle, ClockPortHandle]

  def run(): Unit = {
    
    while (eventQueue.containsTasks) {
     
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


    Reporting.debug(Some(currentTime), "SimController", s"No more active tasks in event queue, simulation complete.")
    
  }

  def handleEvent(event: Event): Unit = {
    event match {
      case Event.ClockEdge(_, clockPort, rising) =>
        if (rising) {
          // clear output cache on clock edges
          outputCache.clear()
          inputDirty.clear()
          clockPort.set(1)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (clockPort.period / 2)).absolute, clockPort, false))
        } else {
          clockPort.set(0)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (clockPort.period / 2)).absolute, clockPort, true))
        }
      case Event.RunTask(_, task, _) =>
        handleTask(task)
    }
  }

  def addClockDomain(clockPortName: String, period: Time, ports: Seq[PortHandle]): ClockPortHandle = {
    val clock = getInputPortHandle(clockPortName).getOrElse {
      Reporting.error(Some(currentTime), "SimController", s"addClockDomain: No input port named ${clockPortName} found in SimModel ${simModel.name}")
      throw new Exception("No such clock port")
    }
    Reporting.debug(Some(currentTime), "SimController", s"Adding clock: ${clock.name} with period ${period}")
    val handle = new SimControllerClockHandle(clock, this, period)
    ports.foreach(p => portToClock(p) = handle)
    eventQueue.enqueue(Event.ClockEdge(currentTime.absolute, handle, false))
    clockHandles(clockPortName) = handle
    handle
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
        val period = clockPort.period
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
        Reporting.debug(Some(currentTime), "SimController", s"Task ${t.name} yielded, waiting for manual resume")

      case Failed(e) =>
        Reporting.error(Some(currentTime), "SimController", s"Active task ${t.name} failed with exception: ${e}")
        throw e
    }
  }

  
  def getInputPortHandle(portName: String): Option[InputPortHandle] = inputHandles.get(portName)

  def getOutputPortHandle(portName: String): Option[OutputPortHandle] = outputHandles.get(portName)

  def getClockPortHandle(portName: String): Option[ClockPortHandle] = clockHandles.get(portName)
  
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
    // if (!port.isInstanceOf[ClockPortHandle] && inputDirty.contains(port)) {
    //   Reporting.error(Some(currentTime), "SimController", s"Overwriting input port ${port.name} after it has been read")
    // }
    inputDirty.add(port)
    inputCache(port) = value
    port.set(value)
  }

  def addTask[T](name: String, order: Int, ctx: Option[CoroutineContext] = None)(block: => T): Task[T] = {
    Reporting.debug(Some(currentTime), "SimController", s"Adding task: ${name} with order ${order}")
    var task: Task[T] = null
    val context = Coroutine.Context.current()
    ctx.foreach(c => taskScope.restoreContext(c))
    task = new Task[T](name, taskScope, order, block)
    Coroutine.Context.restore(context)
    eventQueue.enqueue(Event.RunTask(currentTime, task, order))
    task
  }

  def scheduleTaskAt(time: AbsoluteTime, task: Task[_]): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Scheduling task ${task.name} at time ${time}")
    eventQueue.enqueue(Event.RunTask(time, task, task.order))
  }

  def scheduleTaskNow(task: Task[_]): Unit = scheduleTaskAt(currentTime, task)

  def purgeTask(task: Task[_]): Unit = {
    eventQueue.purgeTask(task)
  }

  def suspendWith(v: SimControllerYield): Unit = {
    taskScope.suspend(Some(v))
  }

  def suspend(): Unit = {
    taskScope.suspend[Unit, SimControllerYield](None)
  }


  def run[T](block: => T): T = SimController.runWith(this) {
    TaskScope
    Task
    val root = Task.root(block)
    this.run()
    root.result.get
  }

}
