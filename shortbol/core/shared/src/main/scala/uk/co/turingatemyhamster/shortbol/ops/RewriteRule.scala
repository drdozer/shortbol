package uk.co.turingatemyhamster.shortbol
package ops

import scalaz.Scalaz._
import scalaz._
import monocle.{Lens, Prism}
import RewriteRule.{MaybeRewritten, Rewritten}
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState

import scala.annotation.implicitNotFound

/**
  * Created by nmrp3 on 09/11/16.
  */
trait RewriteRule[T] {
  self =>

  def apply(t: T): MaybeRewritten[T]

  def eval(t: T): Rewritten[T] = apply(t).fold(_.point[Rewritten], identity)

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
        \/-(r map rr.apply flatMap (_.fold(_.point[Rewritten], identity)))
    }
  }

  def description: Option[String] = None

  def log(name: String): RewriteRule[T] = new RewriteRule[T] {
    override def description = self.description

    private def descriptionString = description getOrElse ""

    override def apply(t: T) = {
      println(s"$name <i> $descriptionString at $t")
      val res = self(t)
      println(s"$name <o> $descriptionString at $res from $t")
      res
    }
  }
}

object RewriteRule {
  implicit class Filtering[S, T](_l: Lens[S, T]) {
    def :==(t: T): RewriteAt[S, S] = RewriteAt.filterRewrite { (s: S) =>
      _l.get(s) == t
    }
  }

  type MaybeRewritten[T] = T \/ Rewritten[T]
  type InstanceExpWriter[T] = Writer[List[longhandAst.InstanceExp], T]
  type Rewritten[T] = Eval.EvalStateT[InstanceExpWriter, T]


  def apply[F, T](f: F)(implicit b: Builder[F, T]): RewriteRule[T] = b apply f

  def noop[T]: RewriteRule[T] = RewriteRule { (t: T) => t.left[Rewritten[T]] }

  @implicitNotFound("Don't know how to build a RewriteRule[${T}] from ${F}")
  trait Builder[F, T] {
    def apply(f: F): RewriteRule[T]
  }

  implicit def fromRewrittenDisjunction[T]: Builder[(T => T \/ Rewritten[T]), T] = new Builder[(T) => Disjunction[T, Rewritten[T]], T] {
    override def apply(f: (T) => Disjunction[T, Rewritten[T]]) = new RewriteRule[T] {
      override def apply(t: T) = f(t)
    }
  }

  implicit def fromStateDisjunction[T]: Builder[(T => T \/ Eval.EvalState[T]), T] = new Builder[(T) => Disjunction[T, EvalState[T]], T] {
    override def apply(f: (T) => Disjunction[T, EvalState[T]]) = new RewriteRule[T] {
      override def apply(t: T) = f(t).map(_.lift : Rewritten[T])
    }
  }

  implicit def fromDisjunction[T]: Builder[(T => T \/ T), T] = new Builder[(T) => Disjunction[T, T], T] {
    override def apply(f: (T) => Disjunction[T, T]) = new RewriteRule[T] {
      override def apply(t: T): MaybeRewritten[T] = f(t).map(_.point[Rewritten])
    }
  }

  implicit def fromState[T]: Builder[T => EvalState[T], T] = new Builder[(T) => EvalState[T], T] {
    override def apply(f: (T) => EvalState[T]) = new RewriteRule[T] {
      override def apply(t: T) = (f(t).lift : Rewritten[T]).right[T]
    }
  }

  implicit def fromFunc[T]: Builder[T => T, T] = new Builder[(T) => T, T] {
    override def apply(f: (T) => T) = new RewriteRule[T] {
      override def apply(t: T) = f(t).point[Rewritten].right[T]
    }
  }

  implicit def fromOptionFunc[T]: Builder[T => Option[T], T] = new Builder[(T) => Option[T], T] {
    override def apply(f: (T) => Option[T]) = RewriteRule { (t: T) =>
      f(t) match {
        case None => t.left[Rewritten[T]]
        case Some(ft) => ft.point[Rewritten].right[T]
      }
    }
  }

  implicit def fromPartialFunc[T]: Builder[PartialFunction[T, T], T] = new Builder[PartialFunction[T, T], T] {
    override def apply(f: PartialFunction[T, T]) = RewriteRule(f.lift)
  }

  implicit def fromFlatMap[T]: Builder[T => RewriteRule[T], T] = new Builder[(T) => RewriteRule[T], T] {
    override def apply(f: (T) => RewriteRule[T]) = RewriteRule { (t: T) =>
      f(t)(t)
    }
  }

  implicit def fromOptionFlatMap[T]: Builder[T => Option[RewriteRule[T]], T] = new Builder[(T) => Option[RewriteRule[T]], T] {

    override def apply(f: (T) => Option[RewriteRule[T]]) = RewriteRule { (t: T) =>
      f(t).map(_ apply t).fold(t.left[Rewritten[T]])(identity)
    }
  }
}

trait RewriteAt[U, T] {
  def apply(rr: RewriteRule[T]): RewriteRule[U]
}

object RewriteAt {

  implicit def rewriteAtLens[S, T](lens: Lens[S, T]): RewriteAt[S, T] = new RewriteAt[S, T] {
    override def apply(rr: RewriteRule[T]) = RewriteRule { (s: S) =>
      val lsets = lens.set(_: T)(s)
      rr(lens.get(s)).bimap(
        lsets,
        _ map lsets
      )
    }
  }

  implicit def rewriteAtPrism[S, T](prism: Prism[S, T]): RewriteAt[S, T] = new RewriteAt[S, T] {
    override def apply(rr: RewriteRule[T]) = RewriteRule { (s: S) =>
      val psets = prism.set(_: T)(s)
      prism.getOrModify(s) flatMap {
        rr apply _ bimap (
          psets,
          _ map psets
        )
      }
    }
  }

  implicit def filterRewrite[T](f: T => Boolean): RewriteAt[T, T] = new RewriteAt[T, T] {
    override def apply(rr: RewriteRule[T]) = RewriteRule { (t: T) =>
      if(f(t)) rr(t)
      else -\/(t)
    }
  }

  implicit def rewriteAtAllElements[T](ae: allElements.type): RewriteAt[List[T], T] = new RewriteAt[List[T], T] {
    override def apply(rr: RewriteRule[T]) = RewriteRule { (ts: List[T]) =>
      val rrts = ts map rr.apply
      if(rrts exists (_.isRight)) {
        (rrts collect {
          case -\/(l) => l.point[Rewritten]
          case \/-(r) => r
        }).sequenceU.right[List[T]]   : RewriteRule.MaybeRewritten[List[T]]
      } else {
        (rrts collect { case -\/(l) => l }).left  : RewriteRule.MaybeRewritten[List[T]]
      }
    }
  }

  object allElements
}

