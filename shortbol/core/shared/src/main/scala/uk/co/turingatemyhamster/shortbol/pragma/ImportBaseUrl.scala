package uk.co.turingatemyhamster.shortbol
package pragma

import ast.{LocalName, Pragma}
import ast.sugar._
import ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.ops.{Eval, EvalContext}

import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object ImportBaseUrl {
  self =>

  def apply: Hook = new Hook {
    override def register(p: Pragma) = List(p).point[EvalState]

    override def ID = self.ID

    override def bootstrap = self.bootstrap
  }

  val all: EvalState[List[Pragma]] = gets((_: EvalContext).prgms.getOrElse(ID, Nil))

  val top: EvalState[Option[Pragma]] = gets((_: EvalContext).prgms.get(ID).flatMap(_.headOption))

  def push(p: Pragma): EvalState[List[Pragma]] = for {
    a <- all
    _ <- modify((_: EvalContext).withPragmas(p))
  } yield a

  def reset(ps: List[Pragma]): EvalState[Unit] = modify((s: EvalContext) => s.copy(prgms = s.prgms + (ID -> ps)))

  def pushFrame[T](p: Pragma)(f: EvalState[T]): EvalState[T] = for {
    a <- push(p)
    t <- f
    _ <- reset(a)
  } yield t

  val ID: LocalName = "importBase"

  val bootstrap: String = "@pragma importBase url"
}
