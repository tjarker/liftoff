package liftoff.verify

import scala.collection.mutable

import liftoff.coroutine.Coroutine
import liftoff.coroutine.CoroutineContext
import liftoff.simulation.task.Task
import scala.reflect.ClassTag
import liftoff.simulation.control.SimController
import liftoff.coroutine.CoroutineContextVariable
import liftoff.misc.Reporting
import liftoff.simulation.Sim
import liftoff.simulation.Time
import liftoff._

case class CompPath(val name: String, val hiearchy: Seq[Component]) {
  override def toString(): String = {
    val hiearchyStr = hiearchy.map(_.name).mkString(".")
    if (hiearchyStr.nonEmpty) {
      s"$hiearchyStr.$name"
    } else {
      name
    }
  }
  
}

abstract class Component {

  val path = Component.currentPath.value.getOrElse {
    throw new Exception("Component created outside of Component.create")
  }
  val parent = path.hiearchy.lastOption
  val children = mutable.Buffer.empty[Component]
  val name = path.name

  Component.currentComponent.value = Some(this) // set this so that child components can find their parent
                                                           // it will be undone by the Component.create method

  Reporting.setProvider(path.toString()) // set reporting provider to this component's path
                                              // will be undone by Component.create method
  // Capture the coroutine context for spawning tasks later on
  val componentContext: CoroutineContext = Coroutine.Context.capture()

  var taskCounter = 0
  val tasks = mutable.Buffer.empty[Task[_]]
  val taskRuntimesMap = mutable.Map.empty[String, Time]

  def createTask[T](block: => T): Task[T] = {
    val name = s"${path}.task[${taskCounter}]"
    taskCounter += 1
    val t = Sim.Scheduler.addTask[T](name, 0, Some(componentContext))(block)
    tasks += t
    t
  }

  def createPhaseTask(phaseName: String)(block: => Unit): Task[Unit] = {
    val name = s"${path}.${phaseName}.task[${taskCounter}]"
    taskCounter += 1
    val t = Sim.Scheduler.addTask[Unit](name, 0, Some(componentContext))(block)
    tasks += t
    t
  }

  override def toString(): String = {
    s"Component(${path.toString()})"
  }

  def cancelTasks(): Unit = {
    tasks.foreach(_.cancelWithChildren())
    tasks.foreach(t => taskRuntimesMap(t.name) = t.getRuntime().ns)
    tasks.clear()
  }

  def joinTasks(): Unit = {
    tasks.foreach(_.join())
    tasks.foreach(t => taskRuntimesMap(t.name) = t.getRuntime().ns)
    tasks.clear()
  }

  def collectTaskRuntimes(): Map[String, Time] = {
    taskRuntimesMap.toMap ++ children.flatMap(_.collectTaskRuntimes()).toMap
  }

  import scala.language.experimental.macros

  def path(f: this.type => Component): String = macro liftoff.macros.Path.pathImpl[this.type]

  
}

case class ComponentBuilder(
  params: Seq[(Config[Any], Any)],
  typeOverrides: Seq[(ClassTag[_], ClassTag[_])]
) {

  def withParam[T](c: Config[T], value: T): ComponentBuilder = {
    copy(params = params :+ (c.asInstanceOf[Config[Any]], value.asInstanceOf[Any]))
  }

  def withTypeOverride[C <: Component: ClassTag, D <: C: ClassTag]: ComponentBuilder = {
    copy(typeOverrides = typeOverrides :+ (implicitly[ClassTag[C]], implicitly[ClassTag[D]]))
  }

  def create[C <: Component: ClassTag](args: Any*)(implicit name: sourcecode.Name): C = {
    val oldOverrides = Component.getOverrides()
    typeOverrides.foreach { case (base, overrideType) =>
      Component.overrideType_(base, overrideType)
    }
    val oldParams = params.map { case (c, v) => c -> Config.swap(c,v) }
    val comp = Component.create[C](args: _*)(implicitly[ClassTag[C]], name)

    Component.restoreOverrides(oldOverrides)
    oldParams.foreach { case (c, v) => Config.set(c, v) }

    comp

  }

}


object Component {

  val overrideMap = new CoroutineContextVariable[mutable.Map[ClassTag[_], ClassTag[_]]](mutable.Map.empty)
  val currentPath = new CoroutineContextVariable[Option[CompPath]](None)
  val currentComponent = new CoroutineContextVariable[Option[Component]](None)

  def create[C <: Component: ClassTag](args: Any*)(implicit name: sourcecode.Name): C = {
    val classTag = overrideMap.value.getOrElse(implicitly[ClassTag[C]], implicitly[ClassTag[C]])
    create(ReflectiveFactory.create[C](classTag)(args: _*))
  }

  def builder: ComponentBuilder = ComponentBuilder(Seq.empty, Seq.empty)

  def create[C <: Component](c: => C)(implicit name: sourcecode.Name): C = {
    val previousProvider = Reporting.getCurrentProvider()
    val path = Component.currentPath.value match {
      case Some(CompPath(_, hiearchy)) => CompPath(name.value, hiearchy :+ Component.currentComponent.value.get)
      case None                        => CompPath(name.value, Seq.empty)
    }
    val comp = currentPath.withValue(Some(path))(c)
    val current = path.hiearchy.lastOption
    current.foreach(_.children += comp)
    Component.currentComponent.value = current
    Reporting.setProvider(previousProvider)
    comp
  }

  def createSeq[C <: Component: ClassTag](n: Int)(args: Int => Seq[Any])(implicit name: sourcecode.Name): Seq[C] = {
    (0 until n).map { i =>
      val idxName: sourcecode.Name = sourcecode.Name(s"${name.value}[$i]")
      create[C](args(i): _*)(implicitly[ClassTag[C]], idxName)
    }
  }

  def overrideType[C <: Component: ClassTag, D <: C: ClassTag]: Unit = {
    val map = overrideMap.value
    map(implicitly[ClassTag[C]]) = implicitly[ClassTag[D]]
  }

  private[liftoff] def overrideType_(base: ClassTag[_], overrideType: ClassTag[_]): Unit = {
    val map = overrideMap.value
    map(base) = overrideType
  }

  def getOverrides(): Map[ClassTag[_], ClassTag[_]] = overrideMap.value.toMap
  def restoreOverrides(overrides: Map[ClassTag[_], ClassTag[_]]): Unit = {
    val map = overrideMap.value
    map.clear()
    overrides.foreach { case (k, v) => map(k) = v }
  }

  def clearOverride[C <: Component: ClassTag]: Unit = {
    val map = overrideMap.value
    map.remove(implicitly[ClassTag[C]])
  }

  import scala.language.experimental.macros

  def path[R <: Component](f: R => Component): String = macro liftoff.macros.Path.pathImpl[R]
}

object ReflectiveFactory {

  def create[T](c: ClassTag[_])(args: Any*): T = {
    val cls = c.runtimeClass

    val argClasses = args.map {
      case null => classOf[Object]
      case a    => a.getClass
    }.toArray

    val ctor = cls.getConstructors.find { c =>
      val params = c.getParameterTypes
      params.length == argClasses.length &&
        params.zip(argClasses).forall { case (p, a) =>
          p.isAssignableFrom(a) ||
          primitiveWrapperMatch(p, a)
        }
    }.getOrElse {
      throw new NoSuchMethodException(
        s"No matching constructor for ${cls.getName}(${argClasses.mkString(", ")})"
      )
    }

    ctor.newInstance(args.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[T]
  }

  private def primitiveWrapperMatch(p: Class[_], a: Class[_]): Boolean =
    (p, a) match {
      case (java.lang.Integer.TYPE, c) if c == classOf[java.lang.Integer] => true
      case (java.lang.Long.TYPE,    c) if c == classOf[java.lang.Long]    => true
      case (java.lang.Double.TYPE,  c) if c == classOf[java.lang.Double]  => true
      case (java.lang.Boolean.TYPE, c) if c == classOf[java.lang.Boolean] => true
      case _ => false
    }
}