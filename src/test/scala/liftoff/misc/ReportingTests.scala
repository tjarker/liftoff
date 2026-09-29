package liftoff.misc

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.misc.Reporting.{Level, Levels}
import liftoff.simulation.DummySimModel
import liftoff.simulation.control.SimController
import liftoff.verify.component.Test

class QuietTest extends Test {
  def test(): Unit = Reporting.info(None, "Hello from the test")
}

class ReportingTests extends AnyWordSpec with Matchers {

  "The Reporting object" should {

    "print formatted reports" in {

      fansi
        .Str(
          Reporting.infoStr(
            Some(liftoff.simulation.Time(123456789, liftoff.simulation.Time.TimeUnit.ns)),
            "A.B.C",
            "This is an info."
          )
        )
        .plainText shouldBe """[info]────@123.457ms─[A.B.C]─────────────────────╢ This is an info.
                              |                                                 ║""".stripMargin

      fansi
        .Str(
          Reporting.warnStr(
            Some(liftoff.simulation.Time(42, liftoff.simulation.Time.TimeUnit.ms)),
            "A.B.C.D",
            "This is a warning."
          )
        )
        .plainText shouldBe """[warn]─────@42.000ms─[A.B.C.D]───────────────────╢ This is a warning.
                              |                                                 ║""".stripMargin

      fansi
        .Str(
          Reporting.errorStr(
            Some(liftoff.simulation.Time(7, liftoff.simulation.Time.TimeUnit.s)),
            "A",
            "This is an error."
          )
        )
        .plainText shouldBe """[error]─────@7.000s──[A]─────────────────────────╢ This is an error.
                              |                                                 ║""".stripMargin

      fansi
        .Str(
          Reporting.successStr(
            Some(liftoff.simulation.Time(999, liftoff.simulation.Time.TimeUnit.us)),
            "A.B",
            "This is a success."
          )
        )
        .plainText shouldBe """[success]─@999.000us─[A.B]───────────────────────╢ This is a success.
                              |                                                 ║""".stripMargin

      fansi
        .Str(
          Reporting.debugStr(
            None,
            "Hello.World",
            "This is a debug."
          )
        )
        .plainText shouldBe """[debug]──────────────[Hello.World]───────────────╢ This is a debug.
                              |                                                 ║""".stripMargin

      fansi
        .Str(Reporting.traceStr(None, "A", "This is a trace."))
        .plainText shouldBe """[trace]──────────────[A]─────────────────────────╢ This is a trace.
                              |                                                 ║""".stripMargin
    }

    "report up to the level of each provider" in {
      val output = new java.io.ByteArrayOutputStream()
      Reporting.withOutput(new java.io.PrintStream(output), colored = false) {
        Reporting.levels.withValue(Levels.default) {
          Reporting.debug(None, "a", "hidden debug")
          Reporting.info(None, "a", "shown info")
          Reporting.success(None, "a", "shown success")
          Reporting.withLevel("a.b", Level.Debug) {
            Reporting.debug(None, "a.b.c", "shown debug")
            Reporting.trace(None, "a.b.c", "hidden trace")
            Reporting.debug(None, "a.bc", "hidden sibling")
          }
          Reporting.debug(None, "a.b.c", "hidden after the scope")
          Reporting.withLevel(Level.Off) {
            Reporting.error(None, "a", "hidden error")
          }
        }
      }
      val text = output.toString
      Seq("shown info", "shown success", "shown debug").foreach(text should include(_))
      text should not include "hidden"
    }

    "take the level of the longest matching provider prefix" in {
      val levels = Levels(Level.Info, Map("a" -> Level.Off, "a.b" -> Level.Trace))
      levels.of("a") shouldBe Level.Off
      levels.of("a.x") shouldBe Level.Off
      levels.of("a.b") shouldBe Level.Trace
      levels.of("a.b.c") shouldBe Level.Trace
      levels.of("ab") shouldBe Level.Info
      levels.of("x") shouldBe Level.Info
    }

    "parse levels as in LIFTOFF_LOG" in {
      Levels.parse("debug") shouldBe Levels(Level.Debug, Map())
      Levels.parse(" warn, liftoff.scheduler=trace ,root.env = OFF") shouldBe Levels(
        Level.Warn,
        Map("liftoff.scheduler" -> Level.Trace, "root.env" -> Level.Off)
      )
      Levels.parse("") shouldBe Levels.default
      an[IllegalArgumentException] should be thrownBy Levels.parse("verbose")
    }

    "show a test run's reports and result by default" in {
      val output = new java.io.ByteArrayOutputStream()
      Reporting.withOutput(new java.io.PrintStream(output), colored = false) {
        Reporting.levels.withValue(Levels.default) {
          new SimController(new DummySimModel).run(Test.run(new QuietTest))
        }
      }
      val text = output.toString
      text should include("Hello from the test")
      text should include("Starting")
      text should include("Finished")
      text should not include "TestPhase..."
      text should not include "Task Name"
    }

  }

}
