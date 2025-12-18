package liftoff

import scopt.Opt

package object config {

  abstract class Field[T] private (val default: Option[T]) {
    def this() = this(None)
    def this(default: T) = this(Some(default))
  }

  abstract class View {
    final def get[T](pname: Field[T]): T = {
      val out = find(pname)
      require(out.isDefined, s"Required config field not found: $pname")
      out.get
    }

    final def tryGet[T](pname: Field[T]): Option[T] = find(pname)

    def find[T](pname: Field[T]): Option[T]
  }
  

  trait Config {
    def key: Object
    def value: Any
    var next: Option[Config]
    def get[T](key: Object): Option[T] = {
      next match {
        case Some(config) if config.key == key =>
          Some(config.value.asInstanceOf[T])
        case Some(nextConfig) =>
          nextConfig.get[T](key)
        case None =>
          None
      }
    }
    def push(c: Config): Config = {
      c.next = Some(this)
      c
    }
    def :+ (c: Config): Config = push(c)
  }


  


}
