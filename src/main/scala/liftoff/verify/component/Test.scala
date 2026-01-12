package liftoff.verify.component

import liftoff.verify.SimPhase
import liftoff.misc.Reporting
import liftoff.verify.Component
import liftoff.verify.TestPhase
import liftoff.verify.Phase
import liftoff.simulation.Sim
import liftoff.verify.ResetPhase
import liftoff.verify.ReportPhase

abstract class Test extends Component with TestPhase {

  override def toString(): String = s"Test(${this.getClass().getSimpleName})"

}

object Test {
  def run(t: => Test): Unit = {
    val root = Component.create(t)
    val testName = root.toString()
    Reporting.info(Some(Sim.time), testName, s"SimPhase...")
    val simPhaseTasks = root.startPhase[SimPhase]()
    Reporting.info(Some(Sim.time), testName, s"ResetPhase...")
    val start = System.nanoTime()
    root.startPhase[ResetPhase]().foreach(_.joinTasks())
    Reporting.info(Some(Sim.time), testName, s"TestPhase...")
    val testStart = System.nanoTime()
    root.startPhase[TestPhase]().foreach(_.joinTasks())
    simPhaseTasks.foreach(_.cancelTasks())
    val simEnd = System.nanoTime()
    Reporting.info(Some(Sim.time), testName, s"ReportPhase...")
    root.startPhase[ReportPhase]().foreach(_.joinTasks())
    val end = System.nanoTime()
    val times = Seq(
      f"ResetPhase: ${(testStart - start) / 1e6}%.2f ms",
      f"TestPhase: ${(simEnd - testStart) / 1e6}%.2f ms",
      f"ReportPhase: ${(end - simEnd) / 1e6}%.2f ms"
    )
    
    Reporting.success(Some(Sim.time), testName, s"Finished\n" + times.mkString("\n"))
  }
}
