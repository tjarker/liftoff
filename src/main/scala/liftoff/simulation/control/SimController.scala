package liftoff.simulation.control

import scala.collection.mutable
import scala.util.DynamicVariable

import liftoff.misc.Reporting
import liftoff.simulation.{SimModel, PortHandle, InputPortHandle, OutputPortHandle, ClockPortHandle}
import liftoff.simulation.Time
import liftoff.simulation.task._
import liftoff.simulation.control.SimControllerYield
import liftoff.simulation.control.SimControllerResponse
import liftoff.coroutine._
import liftoff.simulation.StepUntilResult
import liftoff.simulation.Time._
import liftoff.simulation.EventQueue
import liftoff.simulation.Event

import liftoff.intToTime
import scala.reflect.internal.Reporting
import liftoff.simulation.SimTime



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





class SimController(simModel: SimModel) {

  val taskScope = Coroutine.createScope()

  val eventQueue: EventQueue = new EventQueue
  var currentTime: SimTime = new SimTime(0)

  val outputCache = mutable.Map.empty[CtrlOutHandle, BigInt]
  val inputCache = mutable.Map.empty[CtrlInputHandle, BigInt]
  val inputDirty = mutable.Set.empty[CtrlInputHandle]

  val inputHandles = simModel.inputs.map(p => p.name -> new CtrlInputHandle(p, this)).toMap
  val outputHandles = simModel.outputs.map(p => p.name -> new CtrlOutHandle(p, this)).toMap
  val clockHandles = mutable.Map.empty[String, CtrlClockHandle]
  val clockDomains = mutable.Map.empty[CtrlClockHandle, Seq[CtrlPortHandle]]

  val combinationalOutputToInput = mutable.Map.empty[CtrlOutHandle, Seq[CtrlInputHandle]]

  val clockCycles = mutable.Map.empty[CtrlClockHandle, Int]
  def ports: Seq[PortHandle] = (inputHandles.values ++ outputHandles.values).toSeq

  val portToClock = mutable.Map.empty[CtrlPortHandle, CtrlClockHandle]

  var modelRunTime = 0L
  var taskRunTime = 0L

  def getModelRunTimeMillis(): Double = {
    modelRunTime / 1e6.toDouble
  }

  def getTaskRunTimeMillis(): Double = {
    taskRunTime / 1e6.toDouble
  }

  def getInputPortHandle(portName: String): Option[InputPortHandle] = inputHandles.get(portName)

  def getOutputPortHandle(portName: String): Option[OutputPortHandle] = outputHandles.get(portName)

  def getClockPortHandle(portName: String): Option[ClockPortHandle] = clockHandles.get(portName)

  def addCombinationDependency(output: CtrlOutHandle, inputs: Seq[CtrlInputHandle]): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Adding combinational dependency for output port ${output.name} on input ports ${inputs.map(_.name).mkString(", ")}")
    combinationalOutputToInput(output) = inputs
  }

  def addClockDomain(clockPortName: String, period: Time, ports: Seq[PortHandle]): CtrlClockHandle = {
    require(ports.forall(_.isInstanceOf[CtrlPortHandle]), s"Can't add clock domain with non-CtrlPortHandle ports: ${ports.filterNot(_.isInstanceOf[CtrlPortHandle])}")
    val clock = inputHandles.get(clockPortName).getOrElse {
      Reporting.error(Some(currentTime), "SimController", s"addClockDomain: No input port named ${clockPortName} found in SimModel ${simModel.name}")
      throw new Exception("No such clock port")
    }.backingPort
    Reporting.debug(Some(currentTime), "SimController", s"Adding clock: ${clock.name} with period ${period}")
    val handle = new CtrlClockHandle(clock, this, period)
    ports.foreach(p => portToClock(p.asInstanceOf[CtrlPortHandle]) = handle)
    eventQueue.enqueue(Event.ClockEdge(currentTime.absolute, handle, false))
    clockHandles(clockPortName) = handle
    clockDomains(handle) = ports.asInstanceOf[Seq[CtrlPortHandle]]
    clockCycles(handle) = 0
    handle
  }

  def getCycle(c: CtrlClockHandle): Int = {
    clockCycles.get(c).getOrElse(throw new Exception(s"Clock ${c.name} has no cycle count"))
  }




  def get(port: CtrlPortHandle, isSigned: Boolean): BigInt = {
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
      case iph: CtrlInputHandle =>
        val state = inputCache.getOrElseUpdate(iph, iph.backingPort.get())
        Reporting.debug(Some(currentTime), "SimController", s"Read input port ${iph.name} with value ${state}")
        state
      case oph: CtrlOutHandle =>
        combinationalOutputToInput.get(oph) match {
          case Some(inputs) =>
            Reporting.debug(Some(currentTime), "SimController", s"Output port ${oph.name} has combinational dependencies on input ports: ${inputs.map(_.name).mkString(", ")}")
            val dirty = inputs.exists(inputDirty.contains)
            if (dirty) {
              Reporting.debug(Some(currentTime), "SimController", s"Output port ${oph.name} is dirty due to dependent inputs, evaluating from model")
              val startTime = System.nanoTime()
              simModel.evaluate()
              val endTime = System.nanoTime()
              modelRunTime = modelRunTime + (endTime - startTime)
              val value = oph.backingPort.get()
              outputCache(oph) = value
              value
            } else outputCache.getOrElseUpdate(oph, oph.backingPort.get())
          case None =>
            Reporting.debug(Some(currentTime), "SimController", s"Output port ${oph.name} has no combinational dependencies, reading from cache or model")
            outputCache.getOrElseUpdate(oph, oph.backingPort.get())
        }
      case _ => throw new Exception(s"Unknown port handle type: ${port}")
    }
  }

  def set(port: CtrlInputHandle, value: BigInt): Unit = {
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
    port.backingPort.set(value)
  }






  def nthFallingEdge(clockPort: CtrlClockHandle, cycles: Int): Time = {
    val nextFallingEdge = eventQueue.nextFallingEdge(clockPort).getOrElse {
      Reporting.error(Some(currentTime), "SimController", s"No falling edge scheduled for clock ${clockPort.name}")
      0.fs.absolute
    }
    val period = clockPort.period
    val nextTime = if (nextFallingEdge == currentTime) nextFallingEdge + (period * cycles) else nextFallingEdge + (period * (cycles - 1))
    nextTime
  }

  def handleEvent(event: Event): Unit = {
    event match {
      case Event.ClockEdge(_, clockPort, rising) =>
        if (rising) {
          // clear output cache on clock edges
          outputCache.clear()
          inputCache.clear()
          inputDirty.clear()
          clockPort.set(1)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (clockPort.period / 2)).absolute, clockPort, false))
          val cycle = clockCycles.getOrElse(clockPort, 0) + 1
          Reporting.debug(Some(currentTime), "ClockTick", s"Clock ${clockPort.name} cycle ${cycle}")
          clockCycles(clockPort) = cycle
        } else {
          clockPort.set(0)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (clockPort.period / 2)).absolute, clockPort, true))
        }
      case Event.RunTask(_, task, _) =>
        Reporting.debug(Some(currentTime), "Task", s"${task.name}")
        handleTask(task, EmptyResponse)
        
      
      case Event.CondRunTask(_, task, order, cond@StepUntil(clockPort, port, value, maxCycles), waited) =>
        Reporting.debug(Some(currentTime), "Task", s"Checking conditional run of task ${task.name} (waited ${waited}/${maxCycles})")
        val portValue = port.get()
        if (portValue == value) {
          Reporting.debug(Some(currentTime), "Task", s"Condition met for task ${task.name} (port ${port.name} == ${value}), scheduling task")
          handleTask(task, StepUntilResponse(StepUntilResult.Success(waited)))
        } else if (maxCycles.isDefined && waited >= maxCycles.get) {
          Reporting.error(Some(currentTime), "SimController", s"StepUntil: Reached maxCycles (${maxCycles}) without seeing desired value (${value}) on port ${port.name} for task ${task.name}")
          handleTask(task, StepUntilResponse(StepUntilResult.Timeout(waited)))
        } else {
          Reporting.debug(Some(currentTime), "Task", s"Condition not met for task ${task.name} (port ${port.name} == ${portValue}), rescheduling check")
          val nextFallingEdge = nthFallingEdge(clockPort, 1)
          eventQueue.enqueue(Event.CondRunTask(nextFallingEdge.absolute, task, order, cond, waited + 1))
        }
    }
  }

  def handleTask(t: Task[_], response: SimControllerResponse) = {

    val startTime = System.nanoTime()

    t.runStep(response) match {
      case Finished(result) => // do nothing
        Reporting.debug(Some(currentTime), "SimController", s"Task $t finished")


      case YieldedWith(Step(clockPort, cycles)) =>
        Reporting.debug(Some(currentTime), "SimController", s"Stepping task ${t.name} for ${cycles} cycles on clock ${clockPort}")
        val nextTime = nthFallingEdge(clockPort, cycles)
        Reporting.debug(Some(currentTime), "SimController", s"Scheduling task ${t.name} at time ${nextTime} after ${cycles}x${clockPort.period}")
        eventQueue.enqueue(Event.RunTask(nextTime.absolute, t, t.order))

      case YieldedWith(cond@StepUntil(clockPort, port, value, maxCycles)) =>
        Reporting.debug(Some(currentTime), "SimController", s"Stepping task ${t.name} until port ${port.name} == ${value} on clock ${clockPort} for up to ${maxCycles} cycles")
        val nextFallingEdge = nthFallingEdge(clockPort, 1)
        eventQueue.enqueue(Event.CondRunTask(nextFallingEdge.absolute, t, t.order, cond, 1))

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

    val endTime = System.nanoTime()
    taskRunTime = taskRunTime + (endTime - startTime)
  }

  
  
  
  

  def addTask[T](name: String, order: Int, ctx: Option[CoroutineContext] = None)(block: => T): Task[T] = {
    var task: Task[T] = null
    val context = Coroutine.Context.current()
    ctx.foreach(c => taskScope.restoreContext(c))
    Reporting.debug(Some(currentTime), "SimController", s"Adding task: ${name} with order ${order} and context ${Coroutine.Context.current().pretty}")
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




  def run(): Unit = {
    
    while (eventQueue.containsTasks) {
     
      val event = eventQueue.pop().get

      Reporting.debug(Some(currentTime), "SimController.Loop", s"Handling event: ${event}")

      val delta = event.time - currentTime
      if (delta > 0.fs) {
        val startTime = System.nanoTime()
        simModel.tick(delta.relative)
        val endTime = System.nanoTime()
        outputCache.clear()
        inputCache.clear()
        inputDirty.clear()
        modelRunTime = modelRunTime + (endTime - startTime)
        currentTime = new SimTime(event.time.valueFs)
      }

      handleEvent(event)

      Reporting.debug(Some(currentTime), "SimController.Queue", s"Event queue:\n - ${eventQueue.queue.mkString("\n - ")}")
    }


    Reporting.debug(Some(currentTime), "SimController", s"No more active tasks in event queue, simulation complete.")
    
  }

  def run[T](block: => T): T = SimController.runWith(this) {
    TaskScope
    Task
    val root = Task.root(block)
    this.run()
    root.result.get
  }

}
