package liftoff.coroutine

import scala.collection.mutable
import liftoff.coroutine.CoroutineScope

import jdk.internal.vm.{Continuation, ContinuationScope}

class ContextVariable[T](initial: T) {

  val dynVar = new scala.util.DynamicVariable[T](initial)
  
  def withValue[R](value: T)(block: => R): R = dynVar.withValue(value)(block)

  def withValueScoped[R](scope: CoroutineScope, value: T)(block: => R): R = {
    scope.withContext[T, R](this, value) {
      block
    }
  }

  def value: T = dynVar.value

}
