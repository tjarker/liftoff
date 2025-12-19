package liftoff.verify

import scala.collection.mutable

import liftoff.coroutine.Coroutine

trait Component {

  val context: mutable.Map[AnyRef, Any] = Coroutine.captureLocals()


  
}


object Component {
  def create[C <: Component](c: C)(implicit name: sourcecode.Name): C = ???
}
