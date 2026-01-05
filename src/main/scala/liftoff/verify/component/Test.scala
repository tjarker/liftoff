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
    Reporting.info(Some(Sim.time), s"Starting SimPhase for ${root}")
    val simPhaseTasks = Phase.run[SimPhase](root)
    Reporting.info(Some(Sim.time), s"Starting ResetPhase for ${root}")
    Phase.run[ResetPhase](root).foreach(_._2.join())
    Reporting.info(Some(Sim.time), s"Starting TestPhase for ${root}")
    Phase.run[TestPhase](root).foreach(_._2.join())
    Reporting.info(Some(Sim.time), s"Starting ReportPhase for ${root}")
    Phase.run[ReportPhase](root).foreach(_._2.join())
    simPhaseTasks.foreach(_._2.cancel())
    Reporting.success(Some(Sim.time), s"Finished all phases for ${root}")
  }
}
