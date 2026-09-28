package liftoff.coroutine

import scala.collection.mutable
import liftoff.coroutine.CoroutineScope

import jdk.internal.vm.{Continuation, ContinuationScope}
import liftoff.verify.Component

trait ContextVariable[T] {
  def value: T
  def value_=(newValue: T): Unit
  def withValue[R](value: T)(block: => R): R = {
    val oldValue = this.value
    this.value = value
    try {
      block
    } finally {
      this.value = oldValue
    }
  }
}

class CoroutineContextVariable[T](init: T)(implicit name: sourcecode.Name) extends ContextVariable[T] {

  // Register this context variable in the coroutine context system
  Coroutine.Context.set(this, init)

  // A context the variable was never set in, such as one created before the variable was
  // initialized, sees the initial value.
  def value: T = Coroutine.Context.get[T](this).getOrElse(init)

  def value_=(newValue: T): Unit = {
    Coroutine.Context.set[T](this, newValue)
  }

  override def toString(): String = {
    s"CoroutineContextVariable(${name.value})"
  }

}
