package uk.co.turingatemyhamster.shortbol
package pragma

import ops.EvalContext
import ops.Eval.EvalState

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object Pragma {
  def apply(hooks: Map[String, Hook]): Hook = new Hook {
    override def register(p: ast.Pragma) =
    modify((_: EvalContext).withPHooks(registerHooks))

    def registerHooks(p: ast.Pragma): EvalState[Unit] = p match {
      case ast.Pragma(ast.LocalName("pragma"), ns) if ns.nonEmpty =>
        ns.head match {
          case ast.ValueExp.Identifier(ast.LocalName(name)) =>
            hooks get name match {
              case Some(h) =>
                h.register(p)
              case None =>
                modify((_: EvalContext).withThrown(new NoSuchElementException(s"Could not find pragma hook for $name")))
            }
        }
      case _ =>
        modify((_: EvalContext).withThrown(new IllegalArgumentException(s"Malformed pragma $p")))
    }
  }
}
