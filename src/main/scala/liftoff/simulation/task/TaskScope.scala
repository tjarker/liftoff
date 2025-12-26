package liftoff.simulation.task


object TaskScope {
  val currentScopeVar = new liftoff.coroutine.CoroutineContextVariable[Option[TaskScope]](None)

  def current: Option[TaskScope] = currentScopeVar.value

  def withScope[T](scope: TaskScope)(block: => T): T = {
    currentScopeVar.withValue[T](Some(scope)) {
      block
    }
  }


  def apply[T](block: => T): T = {
    val scope = new TaskScope
    val res = withScope(scope) {
      block
    }
    scope.waitAll()
    res
  }

}

class TaskScope {

  val tasks = scala.collection.mutable.Buffer[Task[_]]()

  def addTask(t: Task[_]): Unit = tasks += t

  def waitAll(): Unit = {
    tasks.foreach(_.join())
  }


}
