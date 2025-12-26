package liftoff.verify

import scala.collection.mutable

import liftoff.coroutine.Coroutine
import liftoff.coroutine.CoroutineContext
import liftoff.simulation.task.Task
import scala.reflect.ClassTag
import liftoff.simulation.SimController
import liftoff.coroutine.CoroutineContextVariable

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

  val context: CoroutineContext = Coroutine.Context.capture()
  val simCtrl = SimController.current
  val path = Coroutine.Context.get[Option[CompPath]](Component.currentPath).get.get
  val name = path.name
  Component.currentComponent.value = Some(this)

  var taskCounter = 0

  def createTask[T](block: => T): Task[T] = {
    val name = s"${path}.task[${taskCounter}]"
    taskCounter += 1
    simCtrl.addTask[T](name, 0, Some(context))(block)
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

  def create[C <: Component](c: => C)(implicit name: sourcecode.Name): C = {
    val path = Component.currentPath.value match {
      case Some(CompPath(_, hiearchy)) => CompPath(name.value, hiearchy :+ Component.currentComponent.value.get)
      case None                        => CompPath(name.value, Seq.empty)
    }
    val comp = currentPath.withValue(Some(path))(c)
    Component.currentComponent.value = path.hiearchy.lastOption
    comp
  }

  def overrideType[C <: Component: ClassTag, D <: C: ClassTag]: Unit = {
    val map = overrideMap.value
    map(implicitly[ClassTag[C]]) = implicitly[ClassTag[D]]
  }

  def clearOverride[C <: Component: ClassTag]: Unit = {
    val map = overrideMap.value
    map.remove(implicitly[ClassTag[C]])
  }
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