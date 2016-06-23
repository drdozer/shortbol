package uk.co.turingatemyhamster.shortbol
package pragma

import ops.EvalContext
import ops.Eval.EvalState
import ast.{Pragma, LocalName}

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object MetaPragma {
  def apply(hooks: Map[String, Hook]): Hook = new Hook {
    override def register(p: Pragma) = for {
      _ <- modify((_: EvalContext).withPHooks(registerHooks))
    } yield List(p)

    def registerHooks(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("pragma"), ns) if ns.nonEmpty =>
        ns.head match {
          case ast.ValueExp.Identifier(ast.LocalName(name)) =>
            hooks get name match {
              case Some(h) =>
                h.register(p)
              case None =>
                for {
                  _ <- modify((_: EvalContext).withThrown(new NoSuchElementException(s"Could not find pragma hook for $name")))
                } yield Nil
            }
        }
      case _ =>
        for {
          _ <- modify((_: EvalContext).withThrown(new IllegalArgumentException(s"Malformed pragma $p")))
        } yield Nil
    }
  }
}
