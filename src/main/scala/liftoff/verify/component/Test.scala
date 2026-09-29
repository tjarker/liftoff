package liftoff.verify.component

import liftoff.verify.SimPhase
import liftoff.misc.Reporting
import liftoff.verify.Component
import liftoff.verify.TestPhase
import liftoff.verify.Phase
import liftoff.simulation.Sim
import liftoff.verify.ResetPhase
import liftoff.verify.ReportPhase
import liftoff.simulation.Time.TimeUnit

/** The root of a testbench; `test()` runs in the test phase. */
abstract class Test extends Component with TestPhase {

  override def toString(): String = s"${this.getClass().getSimpleName}"

}

/** Runs a test: the sim phase throughout, then the reset, test and report phases. */
object Test {
  def run(t: => Test): Unit = {
    val root = Component.create(t)
    val testName = root.toString()
    Reporting.info(Some(Sim.time), testName, s"Starting")
    Reporting.debug(Some(Sim.time), testName, s"SimPhase...")
    val simPhaseTasks = root.startPhase[SimPhase]()
    Reporting.debug(Some(Sim.time), testName, s"ResetPhase...")
    val start = System.nanoTime()
    root.startPhase[ResetPhase]().foreach(_.joinTasks())
    Reporting.debug(Some(Sim.time), testName, s"TestPhase...")
    val testStart = System.nanoTime()
    root.startPhase[TestPhase]().foreach(_.joinTasks())
    simPhaseTasks.foreach(_.cancelTasks())
    val simEnd = System.nanoTime()
    Reporting.debug(Some(Sim.time), testName, s"ReportPhase...")
    root.startPhase[ReportPhase]().foreach(_.joinTasks())
    val end = System.nanoTime()

    Reporting.success(Some(Sim.time), testName, s"Finished")
    Reporting.debug(
      None,
      "liftoff.sim",
      Reporting.table(
        Seq(
          Seq("Phase", "Runtime"),
          Seq("ResetPhase", f"${(testStart - start) / 1e6}%.2f ms"),
          Seq("TestPhase", f"${(simEnd - testStart) / 1e6}%.2f ms"),
          Seq("ReportPhase", f"${(end - simEnd) / 1e6}%.2f ms")
        )
      )
    )

    Reporting.debug(
      None,
      "liftoff.sim",
      Reporting.table(
        Seq(Seq("Task Name", "Runtime")) ++
          root.collectTaskRuntimes().toSeq.sortBy(_._2)(Ordering[liftoff.simulation.Time].reverse).map {
            case (name, time) => Seq(name, time.toString(TimeUnit.ms))
          }
      )
    )
  }
}
