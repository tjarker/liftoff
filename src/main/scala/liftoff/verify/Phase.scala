package liftoff.verify

import scala.collection.mutable

import liftoff.simulation.task.Task
import scala.reflect.ClassTag
import liftoff.misc.Reporting
import liftoff.simulation.Sim

trait Phase { This: Component =>
  def startPhase[P <: Phase: ClassTag](): Seq[Component] = {
    Phase.run[P](this)
  }
}

trait SimPhase extends Phase { This: Component =>
  def sim(): Unit
}

trait ResetPhase extends Phase { This: Component =>
  def reset(): Unit

}

trait TestPhase extends Phase { This: Component =>
  def test(): Unit
}

trait ReportPhase extends Phase { This: Component =>
  def report(): Unit
}

object Phase {

  def getRunner[P <: Phase](ct: ClassTag[P]): P => Unit = {
    ct.runtimeClass match {
      case c if c == classOf[SimPhase]    => (p: P) => p.asInstanceOf[SimPhase].sim()
      case c if c == classOf[ResetPhase]  => (p: P) => p.asInstanceOf[ResetPhase].reset()
      case c if c == classOf[TestPhase]   => (p: P) => p.asInstanceOf[TestPhase].test()
      case c if c == classOf[ReportPhase] => (p: P) => p.asInstanceOf[ReportPhase].report()
      case _  => throw new Exception(s"Unknown phase type: ${ct.runtimeClass}")
    }
  }

  def lookUpPhaseName[P <: Phase](ct: ClassTag[P]): String = {
    ct.runtimeClass match {
      case c if c == classOf[SimPhase]    => "SimPhase"
      case c if c == classOf[ResetPhase]  => "ResetPhase"
      case c if c == classOf[TestPhase]   => "TestPhase"
      case c if c == classOf[ReportPhase] => "ReportPhase"
      case _  => throw new Exception(s"Unknown phase type: ${ct.runtimeClass}")
    }
  }

  def run[P <: Phase: ClassTag](root: Component): Seq[Component] = {
    val ct = implicitly[ClassTag[P]]
    val runner = getRunner(ct)
    def inner(comp: Component, collector: mutable.Buffer[Component]): Unit = {
      comp match {
        case c: P => {
          val phaseName = lookUpPhaseName(ct)
          val task = c.createPhaseTask(phaseName) {
            runner(c)
          }
          collector.append(comp)
        }
        case _ => ()
      }
      comp.children.foreach(child => inner(child, collector))
    }
    val tasks = mutable.Buffer.empty[Component]
    inner(root, tasks)
    tasks.toSeq
  }


}


