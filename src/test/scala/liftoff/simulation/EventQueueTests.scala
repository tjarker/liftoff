package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.Time.IntToTime

class EventQueueTests extends AnyWordSpec with Matchers {

  "An EventQueue" should {
    "order events correctly" in {
      val eq = new EventQueue

      val r1 = Region(1)
      val r2 = Region(2)

      val t1 = 10.ns.absolute
      val t2 = 200.ns.absolute

      val e1 = Event.RunTask(t1, r1, null)
      val e2 = Event.ClockEdge(t1, r1, null, true)
      val e3 = Event.Drive(t1, r2, null, BigInt(0))
      val e4 = Event.RunTask(t2, r2, null)
      val e5 = Event.RunTask(t2, r1, null)
      val e6 = Event.Drive(t1, r1, null, BigInt(1))

      eq.enqueue(e3)
      eq.enqueue(e1)
      eq.enqueue(e5)
      eq.enqueue(e4)
      eq.enqueue(e2)
      eq.enqueue(e6)

      eq.nextTime() shouldEqual Some(t1)
      eq.nextRegion() shouldEqual Some(r1)

      eq.getNextChunk() shouldEqual Seq(e1, e6, e2)
      eq.getNextChunk() shouldEqual Seq(e3)
      eq.getNextChunk() shouldEqual Seq(e5)
      eq.getNextChunk() shouldEqual Seq(e4)

      eq.nextRegion() shouldEqual None
      eq.nextTime() shouldEqual None
    }
  }

}
