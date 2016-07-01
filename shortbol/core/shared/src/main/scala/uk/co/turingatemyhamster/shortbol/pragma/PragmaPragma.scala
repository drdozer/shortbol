package uk.co.turingatemyhamster.shortbol
package pragma

import ops.{EvalContext, LogMessage}
import ops.Eval._
import ast.{LocalName, Pragma}
import ast.sugar._

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object PragmaPragma {
  def apply(hook: Hook*): Hook = new Hook {
    val hooks = hook.map(h => h.ID -> h).toMap

    override def register(p: Pragma) = {
      for {
        _ <- modify((_: EvalContext).withPragmas(p))
        _ <- withPHooks(phook)
      } yield List(p)
    }

    def phook(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("pragma"), ns) if ns.nonEmpty =>
        ns.head match {
          case ast.ValueExp.Identifier(name : ast.LocalName) =>
            hooks get name match {
              case Some(h) =>
                for {
                  _ <- log(LogMessage.info(s"Registering pragma hook for $name", p.region))
                  pp <- h.register(p)
                } yield pp
              case None =>
                for {
                  _ <- log(LogMessage.error(s"Could not find pragma hook for $name. Ignoring it.", name.region))
                } yield Nil
            }
          case bad =>
            for {
              _ <- log(LogMessage.error(s"@pragma declaration malformed for $bad", p.region))
            } yield Nil
        }
      case _ =>
        List(p).point[EvalState]
    }

    override val ID: LocalName = "pragma"

    def _bootstrap: String = "@pragma pragma"
    override val bootstrap = hook.map(_.bootstrap).mkString("\n")
  }
}
