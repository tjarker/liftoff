package liftoff

package object config {
  

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
