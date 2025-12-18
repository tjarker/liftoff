package liftoff.verify

object Config {

  def get[T](c: Config[T]): T = { // type safe retrieval of config value
    ???
  }

}

trait Config[T] {

}