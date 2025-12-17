package mytest

import sbt.testing._

class MyTestRunner(
    args: Array[String],
    remoteArgs: Array[String],
    classLoader: ClassLoader
) extends Runner {
  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    println("Creating tasks") // Debugging line
    println(taskDefs.mkString(", ")) // Debugging line
    taskDefs.map(new MyTestTask(_, classLoader))
  }

  def done(): String = ""

  // Implement these two:
  def args(): Array[String] = args
  def remoteArgs(): Array[String] = remoteArgs
}
