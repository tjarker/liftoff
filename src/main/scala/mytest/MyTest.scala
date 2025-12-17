package mytest

import scala.collection.mutable

abstract class MyTest {
  private val tests = mutable.Buffer.empty[(String, () => Unit)]

  def test(name: String)(body: => Unit): Unit = {
    tests.append((name, () => body))
  }

  def runAll(): Unit = {
    for ((name, body) <- tests) {
      try {
        body()
        println(s"✓ $name")
      } catch {
        case e: Throwable =>
          println(s"✗ $name - ${e.getMessage}")
      }
    }
  }

}
