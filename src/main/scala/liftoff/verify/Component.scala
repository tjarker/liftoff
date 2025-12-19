package liftoff.verify

import scala.collection.mutable

import liftoff.coroutine.Coroutine

trait Component {

  val context: mutable.Map[AnyRef, Any] = Coroutine.currentScope match {
    case Some(scope) => scope.currentLocals
    case None        => mutable.Map()
  }
  
}


object Component {
  def create[C <: Component](c: C): C = ???
}
