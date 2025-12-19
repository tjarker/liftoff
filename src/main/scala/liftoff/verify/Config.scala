package liftoff.verify

import liftoff.coroutine.Coroutine

object Config {

  def tryGet[T](c: Config[T]): Option[T] = {
    Coroutine.getLocal[T](c).orElse(c.default)
  }
  def get[T](c: Config[T]): T = {
    val out = tryGet[T](c)
    require(out.isDefined, s"Required config not found: ${c}")
    out.get
  }
  def getOrElse[T](c: Config[T], default: T): T = {
    val out = tryGet[T](c)
    out.getOrElse(default)
  }
  def set[T](c: Config[T], value: T): Unit = {
    Coroutine.setLocal[T](c, value)
  }

}

abstract class Config[T](val default: Option[T]) {
  def this() = this(None)
  def this(default: T) = this(Some(default))
}