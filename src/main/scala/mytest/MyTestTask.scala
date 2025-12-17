package mytest
import sbt.testing._
import java.util.Optional

class MyTestTask(definition: TaskDef, classLoader: ClassLoader) extends Task {
  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val clazz = classLoader.loadClass(definition.fullyQualifiedName())
    val test = clazz.getDeclaredConstructor().newInstance()

    try {
      val method = clazz.getMethod("runAll")
      method.invoke(test)
      val successEvent = new Event {
        def fullyQualifiedName(): String = definition.fullyQualifiedName()
        def fingerprint(): Fingerprint = definition.fingerprint()
        def selector(): Selector = new TestSelector(definition.fullyQualifiedName())
        def status(): Status = Status.Success
        def throwable(): OptionalThrowable = new OptionalThrowable()
        def duration(): Long = 0L
      }
      handler.handle(successEvent)
    } catch {
      case ex: Throwable =>
        val failEvent = new Event {
          def fullyQualifiedName(): String = definition.fullyQualifiedName()
          def fingerprint(): Fingerprint = definition.fingerprint()
          def selector(): Selector = new TestSelector(definition.fullyQualifiedName())
          def status(): Status = Status.Failure
          def throwable(): OptionalThrowable = new OptionalThrowable(ex)
          def duration(): Long = 0L
        }
        handler.handle(failEvent)
    }

    Array.empty
  }

  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = definition
}
