package liftoff.misc

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ReportingTests extends AnyWordSpec with Matchers {

  "The Reporting object" should {

    "print formatted reports" in {
      // This test just ensures that the report method runs without error.
      // Capturing console output for verification is beyond the scope of this test.
      fansi.Str(Reporting.infoStr(
        liftoff.simulation.Time(123456789, liftoff.simulation.Time.TimeUnit.ns),
        "A.B.C",
        "This is an info."
      )).plainText shouldBe "[info]    @123.457ms [A.B.C]                     This is an info."

      fansi.Str(Reporting.warnStr(
        liftoff.simulation.Time(42, liftoff.simulation.Time.TimeUnit.ms),
        "A.B.C.D",
        "This is a warning."
      )).plainText shouldBe "[warn]     @42.000ms [A.B.C.D]                   This is a warning."

      fansi.Str(Reporting.errorStr(
        liftoff.simulation.Time(7, liftoff.simulation.Time.TimeUnit.s),
        "A",
        "This is an error."
      )).plainText shouldBe "[error]     @7.000s  [A]                         This is an error."

      fansi.Str(Reporting.successStr(
        liftoff.simulation.Time(999, liftoff.simulation.Time.TimeUnit.us),
        "A.B",
        "This is a success."
      )).plainText shouldBe "[success] @999.000us [A.B]                       This is a success."

      fansi.Str(Reporting.debugStr(
        liftoff.simulation.Time(1000, liftoff.simulation.Time.TimeUnit.us),
        "Hello.World",
        "This is a debug."
      )).plainText shouldBe "[debug]     @1.000ms [Hello.World]               This is a debug."
    }

  }

}
