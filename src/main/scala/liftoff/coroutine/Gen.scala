package liftoff.coroutine

import scala.util.DynamicVariable

object Gen {


  def emit[T, F](v: T): Option[F] = {
    val scope = Coroutine.currentScope.getOrElse {
      throw new Exception("Calling emit outside of a generator")
    }
    scope.suspend[F, T](Some(v))
  }


  def apply[T](block: => Unit): Gen[T] = new Gen[T](block)

  def fill[T](n: Int)(block: => T): Gen[T] = {
    Gen[T] {
      for (_ <- 0 until n) {
        emit[T, Any](block)
      }
    }
  }
  def tabulate[T](n: Int)(f: Int => T): Gen[T] = {
    Gen[T] {
      for (i <- 0 until n) {
        emit[T, Any](f(i))
      }
    }
  }
  def range(start: Int, end: Int, step: Int = 1): Gen[Int] = {
    Gen[Int] {
      for (i <- start until end by step) {
        emit[Int, Any](i)
      }
    }
  }

}


class Gen[T](block: => Unit) extends BiGen[Nothing, T](block) with Iterator[T] {

  override def expectsFeedback: Boolean = false

  override def next(): T = {
    val ret = super.next()
    super._feedback(None)
    ret
  }

}


object BiGen {
  def apply[I, O](block: => Unit): BiGen[I, O] = new BiGen[I, O](block)

  def fold[I, O](init: O)(f: (O, I) => O): BiGen[I, O] = {
    BiGen[I, O] {
      var acc = init
      while (true) {
        val input = Gen.emit[O, I](acc)
        acc = f(acc, input.get)
      }
    }
  }

  case class HandshakeException(msg: String) extends RuntimeException(msg)
}


class BiGen[I, O](block: => Unit) {

  val coroutineScope = Coroutine.createScope()
  val coroutine = coroutineScope.create[I, O, Unit](block)

  var nextValue: O = null.asInstanceOf[O]
  var notDone = true

  coroutine.resume(None) match {
    case YieldedWith(v) =>
      nextValue = v
    case Finished(_) =>
      notDone = false
  }

  var openHandshake = false

  def hasNext: Boolean = notDone

  def next(): O = {
    if (openHandshake) {
      throw BiGen.HandshakeException("BiGen handshake not completed. Call feedback() before next().")
    }
    openHandshake = true
    nextValue
  }

  def expectsFeedback: Boolean = openHandshake

  protected def _feedback(in: Option[I]): Unit = {
    if (!openHandshake) {
      throw BiGen.HandshakeException("BiGen handshake not open. Call next() before feedback().")
    }
    coroutine.resume(in) match {
      case YieldedWith(v) =>
        nextValue = v
      case Finished(v) =>
        notDone = false
    }
    openHandshake = false
  }

  def feedback(input: I): Unit = {
    _feedback(Some(input))
  }

  def map(f: O => I): Gen[O] = {
    Gen[O] {
      while (this.hasNext) {
        val out = this.next()
        Gen.emit[O, I](out)
        val in = f(out)
        this.feedback(in)
      }
    }
  }

  def eval(s: Seq[I]): Gen[O] = {
    Gen[O] {
      val it = s.iterator
      while (this.hasNext && it.hasNext) {
        val out = this.next()
        Gen.emit[O, I](out)
        val in = it.next()
        this.feedback(in)
      }
    }
  }

}