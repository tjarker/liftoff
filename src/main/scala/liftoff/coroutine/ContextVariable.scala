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

  def value: T = Coroutine.Context.get[T](this) match {
    case Some(v) => v
    case None    => throw new Exception(s"No intialization for ContextVariable ${name.value} found")
  }


  def value_=(newValue: T): Unit = {
    Coroutine.Context.set[T](this, newValue)
  }

  override def toString(): String = {
    s"CoroutineContextVariable(${name.value})"
  }

}