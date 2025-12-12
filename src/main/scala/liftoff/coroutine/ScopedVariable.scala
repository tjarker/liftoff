package liftoff.coroutine

import scala.collection.mutable
import liftoff.coroutine.CoroutineScope

import jdk.internal.vm.{Continuation, ContinuationScope}

trait ScopedVariable[T] {
  
  def withValue[R](value: T)(block: => R): R

  def value: T

}

class ContinuationScopedVariable[T](scope: ContinuationScope, initial: T) extends ScopedVariable[T] {

  private val map = mutable.Map.empty[Object, T]
  map(Continuation.getCurrentContinuation(scope)) = initial

  def withValue[R](value: T)(block: => R): R = {
    val contextId = Continuation.getCurrentContinuation(scope)
    val oldValue = map.get(contextId)
    map(contextId) = value
    try {
      block
    } finally {
      oldValue match {
        case Some(v) => map(contextId) = v
        case None    => map.remove(contextId)
      }
    }
  }

  def value: T = map(Continuation.getCurrentContinuation(scope))

}

class ThreadedScopedVariable[T](initial: T) extends ScopedVariable[T] {

  private val dynVar = new scala.util.DynamicVariable[T](initial)

  def withValue[R](value: T)(block: => R): R = dynVar.withValue(value)(block)

  def value: T = dynVar.value

}