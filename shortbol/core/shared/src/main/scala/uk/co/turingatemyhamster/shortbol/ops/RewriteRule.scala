package uk.co.turingatemyhamster.shortbol.ops

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
  def noop[T]: RewriteRule[T] = new RewriteRule[T] {
    override def apply(t: T) = -\/(t)
  }
}

trait RewriteAt[U, T] {
  def apply(rr: RewriteRule[T]): RewriteRule[U]
}

