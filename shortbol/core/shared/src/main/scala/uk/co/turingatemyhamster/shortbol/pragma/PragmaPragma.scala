package uk.co.turingatemyhamster.shortbol
package pragma

import ops.{EvalContext, LogMessage}
import ops.Eval.{EvalState, log, constant, withPHooks}
import ast.{LocalName, Pragma}

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object PragmaPragma {
  def apply(hooks: Map[String, Hook]): Hook = new Hook {
    override def register(p: Pragma) = for {
      _ <- withPHooks(phook)
    } yield List(p)

    def phook(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("pragma"), ns) if ns.nonEmpty =>
        ns.head match {
          case ast.ValueExp.Identifier(ast.LocalName(name)) =>
            hooks get name match {
              case Some(h) =>
                for {
                  _ <- log(LogMessage.info(s"Registering pragma hook for $name"))
                  pp <- h.register(p)
                } yield pp
              case None =>
                for {
                  _ <- log(LogMessage.error(s"Could not find pragma hook for $name. Ignoring it."))
                } yield Nil
            }
        }
      case _ =>
        constant(List(p))
    }
  }
}
