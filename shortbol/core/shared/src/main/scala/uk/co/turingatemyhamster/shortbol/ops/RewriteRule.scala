package uk.co.turingatemyhamster.shortbol.ops

import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState

import scalaz.Scalaz._
import scalaz._

/**
  * Created by nmrp3 on 09/11/16.
  */
trait RewriteRule[T] {
  self =>

  def apply(t: T): T \/ Eval.EvalState[T]

  def at[AT, U](at: AT)(implicit rwAt: AT => RewriteAt[U, T]): RewriteRule[U] = rwAt(at)(this)

  def or(rr: RewriteRule[T]): RewriteRule[T] = new RewriteRule[T] {
    override def apply(t: T) = self apply t match {
      case -\/(l) =>
        rr(l)
      case r @ \/-(_) =>
        r
    }
  }

  def andThen(rr: RewriteRule[T]): RewriteRule[T] = new RewriteRule[T] {
    override def apply(t: T) = self(t) match {
      case -\/(l) =>
        rr apply l
      case \/-(r) =>
        \/-(r map rr.apply flatMap (_.fold(_.point[Eval.EvalState], identity)))
    }
  }
}

object RewriteRule {
  type Rewritten[T] = T \/ EvalState[T]

  def apply[F, T](f: F)(implicit b: Builder[F, T]): RewriteRule[T] = b apply f

  def noop[T]: RewriteRule[T] = RewriteRule { (t: T) => t.left[EvalState[T]] }

  trait Builder[F, T] {
    def apply(f: F): RewriteRule[T]
  }

  implicit def fromFunc[T]: Builder[(T => T \/ EvalState[T]), T] = new Builder[(T) => Disjunction[T, EvalState[T]], T] {
    override def apply(f: (T) => Disjunction[T, EvalState[T]]) = new RewriteRule[T] {
      override def apply(t: T) = f(t)
    }
  }

  implicit def fromOptionFunc[T]: Builder[T => Option[T], T] = new Builder[(T) => Option[T], T] {
    override def apply(f: (T) => Option[T]) = RewriteRule { (t: T) =>
      f(t) match {
        case None => t.left[EvalState[T]]
        case Some(ft) => ft.point[EvalState].right[T]
      }
    }
  }

  implicit def fromPartialFunc[T]: Builder[PartialFunction[T, T], T] = new Builder[PartialFunction[T, T], T] {
    override def apply(f: PartialFunction[T, T]) = RewriteRule(f.lift)
  }
}

trait RewriteAt[U, T] {
  def apply(rr: RewriteRule[T]): RewriteRule[U]
}

