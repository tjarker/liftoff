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


// class ContextVariable[T](initial: T) {

//   val dynVar = new scala.util.DynamicVariable[T](initial)

//   val x = new scala.util.DynamicVariable[Int](0)
//   x.value = 10
  
//   def withValue[R](value: T)(block: => R): R = dynVar.withValue(value)(block)

//   def withValueScoped[R](scope: CoroutineScope, value: T)(block: => R): R = {
//     scope.withContext[T, R](this, value) {
//       block
//     }
//   }

//   def value: T = dynVar.value

// }

// object ContextVariable {
//   val registry: mutable.ListBuffer[ContextVariable[?]] = mutable.ListBuffer()

//   def init[T](initial: T): ContextVariable[T] = {
//     val ctxVar = new ContextVariable[T](initial)
//     registry.append(ctxVar)
//     ctxVar
//   }

// }


object InheritableCoroutineLocal {

  val registry: mutable.ListBuffer[InheritableCoroutineLocal[?]] = mutable.ListBuffer()

  def register[T](ctxVar: InheritableCoroutineLocal[T]): Unit = registry.append(ctxVar)

}

class InheritableCoroutineLocal[T](init: T) extends ContextVariable[T] {

  val inheritableThreadLocal = new InheritableThreadLocal[T]() {
    override def initialValue(): T = init
  }

  def value: T = Coroutine.currentScope match {
    case Some(coro) => Coroutine.getLocal[T](this) match {
      case Some(v) => v
      case None    => inheritableThreadLocal.get()
    }
    case None       => inheritableThreadLocal.get()
  }


  def value_=(newValue: T): Unit = Coroutine.currentScope match {
    case Some(scope) => scope.setLocal[T](this, newValue)
    case None       => inheritableThreadLocal.set(newValue)
  }

}