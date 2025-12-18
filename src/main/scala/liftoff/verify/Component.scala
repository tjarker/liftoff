package liftoff.verify

trait Component {
  
}


object Component {
  def create[C <: Component](c: C): C = ???
}
