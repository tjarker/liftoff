package liftoff.simulation

import liftoff.simulation.Time._
import liftoff.coroutine.Coroutine
import liftoff.coroutine.{Finished, YieldedWith, Failed}
import scala.util.DynamicVariable
import liftoff.misc.Reporting

trait SimControllerYield
case class Step(cycles: Int) extends SimControllerYield


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

  val taskScope = Coroutine.defaultScope

  val eventQueue: EventQueue = new EventQueue
  var currentTime: Time.AbsoluteTime = 0.fs.absolute

  // TODO: clear output cache on clock edges
  val outputCache = collection.mutable.Map.empty[OutputPortHandle, BigInt]
  val inputCache = collection.mutable.Map.empty[InputPortHandle, BigInt]

  def run(): Unit = {
    
    while (eventQueue.containsTasks) {
      val events = eventQueue.getNextChunk()
      assert(events.nonEmpty)

      val delta = events.head.time - currentTime
      currentTime = events.head.time

      Reporting.info(Some(currentTime), "SimController",s"SimController advancing time from $currentTime to ${events.head.time} with ${events.size} events")
      Reporting.info(Some(currentTime), "SimController",s"Events: \n - ${events.mkString("\n - ")}")

      
      simModel.tick(delta.relative)
      
      for (event <- events) {
        event match {
          case Event.Drive(_, _, inputPort, value) =>
            set(inputPort, value)
          case Event.ClockEdge(_, _, clockPort, rising) =>
            // clear output cache on clock edges
            if (rising) {
              outputCache.clear()
              clockPort.set(1)
              eventQueue.enqueue(Event.ClockEdge((currentTime + 10.ns).absolute, Region(0), clockPort, false))
            } else {
              clockPort.set(0)
              eventQueue.enqueue(Event.ClockEdge((currentTime + 10.ns).absolute, Region(0), clockPort, true))
            }
          case Event.RunTask(_, _, task) =>
            task.runStep() match {
              case Finished(result) => // do nothing
              case YieldedWith(Step(cycles)) =>
                val nextTime = currentTime + (10.ns * cycles).relative
                eventQueue.enqueue(Event.RunTask(nextTime.absolute, task.region, task))
              case y => throw new Exception("Unexpected yield from task in SimController: " + y)
            }

        }
      }
    }
    
  }

  def getInputPortHandle(portName: String): InputPortHandle = {
    new SimControllerInputHandle(simModel.getInputPortHandle(portName), this)
  }

  def getOutputPortHandle(portName: String): OutputPortHandle = {
    new SimControllerOutputHandle(simModel.getOutputPortHandle(portName), this)
  }

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
    //eventQueue.enqueue(Event.Drive(currentTime, Region(0), port, value))
  }

  def addTask(region: Region, block: => Unit): Unit = {
    eventQueue.enqueue(Event.RunTask(currentTime, region, new Task[Unit](s"task@r${region.id}", region, taskScope, block)))
  }

}
