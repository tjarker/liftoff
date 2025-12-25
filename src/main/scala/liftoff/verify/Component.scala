package liftoff.verify

import scala.collection.mutable

import liftoff.coroutine.Coroutine
import liftoff.coroutine.CoroutineContext

trait Component {

  val context: CoroutineContext = Coroutine.Context.capture()

  def createTask = ???
  
}


object Component {
  def create[C <: Component](c: C)(implicit name: sourcecode.Name): C = {
    ???
  }
}
