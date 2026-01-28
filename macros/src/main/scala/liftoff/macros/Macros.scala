package liftoff.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Path {

  def path[A](f: A => Any): String = macro pathImpl[A]

  def pathImpl[A: c.WeakTypeTag](
    c: blackbox.Context
  )(f: c.Expr[A => Any]): c.Expr[String] = {
    import c.universe._

    def collect(t: Tree, acc: List[String]): List[String] = t match {
      case Select(qual, name) =>
        collect(qual, name.decodedName.toString :: acc)

      case Ident(_) =>
        acc

      case Function(_, body) =>
        collect(body, acc)

      case Block(_, expr) =>
        collect(expr, acc)

      case _ =>
        c.abort(
          c.enclosingPosition,
          "path must be a pure field selection (e.g. _.a.b.c)"
        )
    }

    val elems = collect(f.tree, Nil)
    c.Expr[String](Literal(Constant(elems.mkString("."))))
  }
}
