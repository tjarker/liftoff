package liftoff.simulation

import liftoff.simulation.Time._
import liftoff.coroutine.Coroutine
import liftoff.coroutine.{Finished, YieldedWith, Failed}
import scala.util.DynamicVariable
import liftoff.misc.Reporting
import liftoff.coroutine.Yielded

trait SimControllerYield
case class Step(clockPort: InputPortHandle, cycles: Int) extends SimControllerYield


/* 

  Dynamic variables don't seem to work for continuations

  idea:
    - we need unique identifiers for each task
      - could use getCurrentThread for threaded backends
      - could use Continuation.getCurrentContinuation for continuation backend
    - store current SimController in a map from task id to SimController

 */

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

  // TODO: clear output cache on clock edges
  val outputCache = collection.mutable.Map.empty[OutputPortHandle, BigInt]
  val inputCache = collection.mutable.Map.empty[InputPortHandle, BigInt]
  val inputDirty = collection.mutable.Set.empty[InputPortHandle]


  val clockPeriods = collection.mutable.Map.empty[InputPortHandle, Time]

  def run(): Unit = {
    
    while (eventQueue.containsActiveTasks) {
     
      val event = eventQueue.pop().get

      Reporting.debug(Some(currentTime), "SimController", s"Handling event: ${event}")
      Reporting.debug(Some(currentTime), "SimController", s"Event queue: ${eventQueue}")
      val delta = event.time - currentTime
      if (delta > 0.fs) {
        simModel.tick(delta.relative)
        currentTime = event.time
      }

      handleEvent(event)
    }
    
  }

  def handleEvent(event: Event): Unit = {
    event match {
      case Event.ClockEdge(_, clockPort, period, rising) =>
        if (rising) {
          // clear output cache on clock edges
          outputCache.clear()
          clockPort.set(1)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (period / 2)).absolute, clockPort, period, false))
        } else {
          clockPort.set(0)
          eventQueue.enqueue(Event.ClockEdge((currentTime + (period / 2)).absolute, clockPort, period, true))
        }
      case Event.RunActiveTask(_, task) =>
        handleTask(task)
      case Event.RunInactiveTask(_, task) =>
        handleTask(task)
    }
  }

  def addClock(clock: InputPortHandle, period: Time): Unit = {
    Reporting.debug(Some(currentTime), "SimController", s"Adding clock: ${clock.name} with period ${period}")
    clockPeriods(clock) = period
    eventQueue.enqueue(Event.ClockEdge(currentTime.absolute, clock, period, false))
  }

  def handleTask(t: Task[_]) = {
    t.runStep() match {
          case Finished(result) => // do nothing
            Reporting.debug(Some(currentTime), "SimController", s"Task $t finished")
          case YieldedWith(Step(clockPort, cycles)) =>

            Reporting.debug(Some(currentTime), "SimController", s"Stepping task ${t.name} for ${cycles} cycles on clock ${clockPort}")
            val nextFallingEdge = eventQueue.nextEdgeFalling(clockPort).getOrElse {
              Reporting.error(Some(currentTime), "SimController", s"No falling edge scheduled for clock ${clockPort.name} when trying to step task ${t.name}")
              0.fs.absolute
            }
            val period = clockPeriods.getOrElse(clockPort, {
              Reporting.error(Some(currentTime), "SimController", s"No period recorded for clock ${clockPort.name} when trying to step task ${t.name}")
              throw new Exception("No clock period")
            })
            val nextTime = if (nextFallingEdge == currentTime) nextFallingEdge + (period * cycles) else nextFallingEdge + (period * (cycles - 1))
            eventQueue.enqueue(Event.RunActiveTask(nextTime.absolute, t))
          case Yielded => // do nothing, will be resumed manually
          case Failed(e) =>
            Reporting.error(Some(currentTime), "SimController", s"Active task ${t.name} failed with exception: ${e}")
            throw e
        }
  }

  val inputHandles = simModel.inputs.map(p => p.name -> new SimControllerInputHandle(p, this)).toMap
  val outputHandles = simModel.outputs.map(p => p.name -> new SimControllerOutputHandle(p, this)).toMap

  def getInputPortHandle(portName: String): Option[InputPortHandle] = inputHandles.get(portName)

  def getOutputPortHandle(portName: String): Option[OutputPortHandle] = outputHandles.get(portName)
  
  def get(port: PortHandle, isSigned: Boolean): BigInt = {
    port match {
      case iph: InputPortHandle =>
        inputCache.getOrElseUpdate(iph, iph.get())
      case oph: OutputPortHandle =>
        outputCache.getOrElseUpdate(oph, oph.get())
    }
  }

  def set(port: InputPortHandle, value: BigInt): Unit = {
    inputCache(port) = value
    port.set(value)
  }

  def addActiveTask(name: String)(block: => Unit): Unit = {
    val task = new Task[Unit](name, taskScope, {
      block
    })
    eventQueue.enqueue(Event.RunActiveTask(currentTime, task))
  }

  def addInactiveTask(name: String)(block: => Unit): Unit = {
    val task = new Task[Unit](name, taskScope, {
      block
    })
    eventQueue.enqueue(Event.RunInactiveTask(currentTime, task))
  }

  def scheduleActiveTaskAt(time: AbsoluteTime, block: => Unit): Unit = {
    eventQueue.enqueue(Event.RunActiveTask(time, new Task[Unit](s"task", taskScope, block)))
  }
  def scheduleActiveTaskNow(block: => Unit): Unit = {
    eventQueue.enqueue(Event.RunActiveTask(currentTime, new Task[Unit](s"task", taskScope, block)))
  }
  def scheduleInactiveTaskAt(time: AbsoluteTime, block: => Unit): Unit = {
    eventQueue.enqueue(Event.RunInactiveTask(time, new Task[Unit](s"task", taskScope, block)))
  }
  def scheduleInactiveTaskNow(block: => Unit): Unit = {
    eventQueue.enqueue(Event.RunInactiveTask(currentTime, new Task[Unit](s"task", taskScope, block)))
  }

  def suspendWith(v: SimControllerYield): Unit = {
    taskScope.suspendWith(v)
  }

}
