package liftoff.coroutine

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GeneratorTests extends AnyWordSpec with Matchers {
  
  "A Generator" should {
    "produce values correctly" in {
      val gen = Gen[Int] {
        for (i <- 1 to 5) Gen.emit(i * 10) 
      }

      gen.toSeq shouldEqual Seq(10, 20, 30, 40, 50)
    }

    "work with empty generators" in {
      val gen = Gen[Int] {
        // No emissions
      }

      gen.toSeq shouldEqual Seq()
    }

    "work with infinite generators" in {
      val gen = Gen[Int] {
        var i = 0
        while (true) {
          Gen.emit(i)
          i += 1
        }
      }

      gen.take(5).toSeq shouldEqual Seq(0, 1, 2, 3, 4)
    }

    "work with fibonacci sequence" in {
      val fibGen = Gen[Int] {
        var a = 0
        var b = 1
        while (true) {
          Gen.emit(a)
          val next = a + b
          a = b
          b = next
        }
      }

      fibGen.take(10).toSeq shouldEqual Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    "provide map functionality" in {
      val gen = Gen[Int] {
        for (i <- 1 to 5) Gen.emit(i)
      }

      val mappedGen = gen.map(_ * 100)
      mappedGen.toSeq shouldEqual Seq(100, 200, 300, 400, 500)
    }

    "work with filter functionality" in {
      val gen = Gen[Int] {
        for (i <- 1 to 10) Gen.emit(i)
      }

      val filteredGen = gen.filter(_ % 2 == 0)
      filteredGen.toSeq shouldEqual Seq(2, 4, 6, 8, 10)
    }

    "work with Gen.fill" in {
      var i = 0
      val gen = Gen.fill(5)({
        i += 1
        i
      })
      gen.toSeq shouldEqual Seq(1, 2, 3, 4, 5)
    }

    "work with Gen.tabulate" in {
      val gen = Gen.tabulate(5)(i => i * i)
      gen.toSeq shouldEqual Seq(0, 1, 4, 9, 16)
    }

    "work with Gen.range" in {
      val gen = Gen.range(3, 8, step = 2)
      gen.toSeq shouldEqual Seq(3, 5, 7)
    }
  }

  "A Generator with feedback" should {
    "work with empty generators" in {
      val gen = BiGen[Int, Int] {
        // No emissions
      }

      gen.map(a => a).toSeq shouldEqual Seq[Int]()
    }

    "provide map functionality" in {
      val gen = BiGen[Int, Int] {
        var v = Gen.emit[Int, Int](1).get
        while (true) {
          v = Gen.emit[Int, Int](v + 1).get
        }
      }

      val results = gen.map(_ * 10).take(5).toSeq
      results shouldEqual Seq(1, 11, 111, 1111, 11111)
    }

    "allow for evaluation using sequences" in {
      val gen = BiGen[Int, Int] {
        var v = Gen.emit[Int, Int](0).get
        while (true) {
          v = Gen.emit[Int, Int](v + 1).get
        }
      }

      val inputSeq = Seq(5, 10, 15, 20, 25)
      val results = gen.eval(inputSeq).toSeq
      results shouldEqual Seq(0, 6, 11, 16, 21)
    }

    "receive feedback" in {
      val gen = BiGen[Int, Int] {
        var v = Gen.emit[Int, Int](0).get
        while (true) {
          v = Gen.emit[Int, Int](v * 2).get
        }
      }
      gen.next() shouldBe 0
      gen.feedback(5)
      gen.next() shouldBe 10
      gen.feedback(3)
      gen.next() shouldBe 6
      gen.feedback(7)
      gen.next() shouldBe 14
    }

    "enforce handshake order" in {
      val gen = BiGen[Int, Int] {
        var v = Gen.emit[Int, Int](0).get
        while (true) {
          v = Gen.emit[Int, Int](v * 2).get
        }
      }

      // Calling feedback before next should throw exception
      an[BiGen.HandshakeException] should be thrownBy {
        gen.feedback(5)
      }

      gen.next() shouldBe 0

      // Calling next before feedback should throw exception
      an[BiGen.HandshakeException] should be thrownBy {
        gen.next()
      }

      gen.feedback(5)
      gen.next() shouldBe 10
    }

  }

}
