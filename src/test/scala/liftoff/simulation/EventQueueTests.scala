package liftoff.simulation

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import liftoff.simulation.Time.IntToTime

class EventQueueTests extends AnyWordSpec with Matchers {

  "An EventQueue" should {
    "order events correctly" in {
      val eq = new EventQueue

      val t1 = 10.ns.absolute
      val t2 = 200.ns.absolute

      val e1 = Event.RunTask(t1, null, 10)
      val e2 = Event.ClockEdge(t1, null, true)
      val e3 = Event.RunTask(t2, null, Int.MaxValue)
      val e4 = Event.RunTask(t2, null, 0)
      val e5 = Event.RunTask(t2, null, 0)
      val e6 = Event.ClockEdge(t1, null, false)
      eq.enqueue(e3)
      eq.enqueue(e1)
      eq.enqueue(e5)
      eq.enqueue(e4)
      eq.enqueue(e2)
      eq.enqueue(e6)

      eq.nextTime() shouldEqual Some(t1)
      eq.pop() shouldEqual Some(e1)
      eq.pop() shouldEqual Some(e2)
      eq.pop() shouldEqual Some(e6)
      eq.nextTime() shouldEqual Some(t2)
      eq.pop() shouldEqual Some(e4)
      eq.pop() shouldEqual Some(e5)
      eq.pop() shouldEqual Some(e3)

    }
  }

}
