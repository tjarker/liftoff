package liftoff.misc

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ReportingTests extends AnyWordSpec with Matchers {

  "The Reporting object" should {

    "print formatted reports" in {

      fansi.Str(Reporting.infoStr(
        Some(liftoff.simulation.Time(123456789, liftoff.simulation.Time.TimeUnit.ns)),
        "A.B.C",
        "This is an info."
      )).plainText shouldBe """[info]────@123.457ms─[A.B.C]─────────────────────╢ This is an info.
                              |                                                 ║""".stripMargin

      fansi.Str(Reporting.warnStr(
        Some(liftoff.simulation.Time(42, liftoff.simulation.Time.TimeUnit.ms)),
        "A.B.C.D",
        "This is a warning."
      )).plainText shouldBe """[warn]─────@42.000ms─[A.B.C.D]───────────────────╢ This is a warning.
                              |                                                 ║""".stripMargin         

      fansi.Str(Reporting.errorStr(
        Some(liftoff.simulation.Time(7, liftoff.simulation.Time.TimeUnit.s)),
        "A",
        "This is an error."
      )).plainText shouldBe """[error]─────@7.000s──[A]─────────────────────────╢ This is an error.
                              |                                                 ║""".stripMargin

      fansi.Str(Reporting.successStr(
        Some(liftoff.simulation.Time(999, liftoff.simulation.Time.TimeUnit.us)),
        "A.B",
        "This is a success."
      )).plainText shouldBe """[success]─@999.000us─[A.B]───────────────────────╢ This is a success.
                              |                                                 ║""".stripMargin

      fansi.Str(Reporting.debugStr(
        None,
        "Hello.World",
        "This is a debug."
      )).plainText shouldBe """[debug]──────────────[Hello.World]───────────────╢ This is a debug.
                              |                                                 ║""".stripMargin
    }

  }

}
