package uk.co.turingatemyhamster.shortbol
package ops

import scalaz.Scalaz._
import scalaz._
import monocle.{Lens, Prism}
import RewriteRule.{FilteringEq, Logging, MaybeRewritten, Rewritten}
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.sharedAst.{Identifier, Literal}

import scala.annotation.implicitNotFound

/**
  * Created by nmrp3 on 09/11/16.
  */
trait RewriteRule[T] {
  self =>

  def apply(t: T): MaybeRewritten[T]

//  def eval(t: T): Rewritten[T] = apply(t).fold(_.point[Rewritten], identity)

  def at[AT, U](at: AT)(implicit rwAt: RewriteAtBuilder[AT, U, T]): RewriteRule[U] = rwAt(at)(this)

  def or(rr: RewriteRule[T]): RewriteRule[T] = new RewriteRule[T] {
    override def apply(t: T) = for {
      selfRw <- self apply t
      rrRw <- selfRw match {
        case -\/(l) =>
          rr apply l
        case r@ \/-(_) =>
          r.point[EvalState]
      }
    } yield rrRw
  }

  def andThen(rr: RewriteRule[T]): RewriteRule[T] = new RewriteRule[T] {
    override def apply(t: T) = for {
      selfRw <- self apply t
      rrRw <- selfRw fold (
        rr apply _,
        _.run match {
          case (extras, t2) => for {
            t2Rw <- rr apply t2
          } yield t2Rw fold (
              _.set(extras).right,
              _.run match {
                case (t2ex, t3) =>
                  t3.set(extras ++ t2ex).right
              }
            )
        }
      )
    } yield rrRw
  }

  def description: Option[String] = None

  def log(name: String): RewriteRule[T] = new RewriteRule[T] {
    override def description = self.description

    private def descriptionString = description getOrElse ""

    override def apply(t: T) = for {
      res <- self apply t
      _ = println(s"$name <i> $descriptionString at $t")
      _ = println(s"$name <o> $descriptionString ${res.fold(_ => "unchanged", _ => "rewritten")} at $t")
    } yield res
  }
}

object RewriteRule {
  implicit class FilteringOps[S, T](_l: Lens[S, T]) {
    def :==(t: T) = FilteringEq[S, T](_l, t)
  }

  case class FilteringEq[S, T](l: Lens[S, T], t: T)

  implicit class LoggingOps[T](_t: T) {
    def log(msg: String): Logging[T] = Logging(msg, _t)
  }

  case class Logging[T](msg: String, t: T)

  def ofType[I](i: I) = OfType(i)

  case class OfType[I](i: I)

  type MaybeRewritten[T] = Eval.EvalState[T \/ Rewritten[T]]
  type Rewritten[T] = Writer[List[longhandAst.InstanceExp], T]

//  def Rewritten[T](w: InstanceExpWriter[T]): Rewritten[T] =
//    IndexedStateT[InstanceExpWriter, EvalContext, EvalContext, T](s => w.map(s -> _))

  def rewrite(r: RewriteRule[longhandAst.SBFile], sf: longhandAst.SBFile): EvalState[longhandAst.SBFile] = {
    var depth = 0
    def rewriteStep(sf: longhandAst.SBFile): EvalState[longhandAst.SBFile] = {
      if(depth > 3) throw new IllegalStateException("Recursed too deeply during rewrite rules. Perhaps a rewrite rule is misfiring?")
      depth += 1
      for {
        rsf <- r(sf)
        res <- rsf.fold(_.point[EvalState],
          _.run match {
            case (extras, newSf) =>
              rewriteStep(longhandAst.SBFile(newSf.tops ::: extras))
          }
        )
      } yield res
    }
    rewriteStep(sf)
  }

  def apply[F, T](f: F)(implicit b: Builder[F, T]): RewriteRule[T] = b apply f

  def noop[T]: RewriteRule[T] = RewriteRule { (t: T) => t.left[Rewritten[T]] }

  @implicitNotFound("Don't know how to build a RewriteRule[${T}] from ${F}")
  trait Builder[F, T] {
    def apply(f: F): RewriteRule[T]
  }

  implicit def fromMaybeRewritten[T]: Builder[T => MaybeRewritten[T], T] = new Builder[(T) => MaybeRewritten[T], T] {
    override def apply(f: (T) => MaybeRewritten[T]) = new RewriteRule[T] {
      override def apply(t: T) = f(t)
    }
  }

  implicit def fromRewrittenDisjunction[T]: Builder[(T => T \/ Rewritten[T]), T] = new Builder[(T) => Disjunction[T, Rewritten[T]], T] {
    override def apply(f: (T) => Disjunction[T, Rewritten[T]]) = RewriteRule { (t: T) =>
      f(t).point[EvalState]
    }
  }

  implicit def fromDisjunction[T]: Builder[(T => T \/ T), T] = new Builder[(T) => Disjunction[T, T], T] {
    override def apply(f: (T) => Disjunction[T, T]) = RewriteRule { (t: T) =>
      f(t).map(_.point[Rewritten])
    }
  }

  implicit def fromState[T]: Builder[T => EvalState[T], T] = new Builder[(T) => EvalState[T], T] {
    override def apply(f: (T) => EvalState[T]) = RewriteRule { (t: T) =>
      for {
        ft <- f(t)
      } yield ft.point[Rewritten].right[T]
    }
  }

  implicit def fromFunc[T]: Builder[T => T, T] = new Builder[(T) => T, T] {
    override def apply(f: (T) => T) = RewriteRule { (t: T) =>
      f(t).right[T]
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
      (f(t): RewriteRule[T])(t)
    }
  }

  implicit def fromStateFlatMap[T]: Builder[T => EvalState[RewriteRule[T]], T] = new Builder[(T) => EvalState[RewriteRule[T]], T] {
    override def apply(f: (T) => EvalState[RewriteRule[T]]) = RewriteRule { (t: T) =>
      for {
        rr <- f(t)
        res <- rr(t)
      } yield res
    }
  }

  implicit def fromOptionFlatMap[T]: Builder[T => Option[RewriteRule[T]], T] = new Builder[(T) => Option[RewriteRule[T]], T] {
    override def apply(f: (T) => Option[RewriteRule[T]]) = RewriteRule { (t: T) =>
      f(t) match {
        case None => t.left[Rewritten[T]].point[EvalState]
        case Some(rr) => rr(t)
      }
    }
  }

  object allElements

  object *
}

@implicitNotFound("Don't know how to use a ${AT} to rewrite ${T} at ${U}")
trait RewriteAtBuilder[AT, U, T] {
  def apply(at: AT): RewriteAt[U, T]
}

@implicitNotFound("Don't know how to rewrite ${T} at ${U}")
trait RewriteAt[U, T] {
  def apply(rr: RewriteRule[T]): RewriteRule[U]
}

object RewriteAtBuilder {

  implicit def rewriteAtLog[AT, S, T](rab: RewriteAtBuilder[AT, S, T]): RewriteAtBuilder[RewriteRule.Logging[AT], S, T] = new RewriteAtBuilder[Logging[AT], S, T] {
    override def apply(at: Logging[AT]) = new RewriteAt[S, T] {
      override def apply(rr: RewriteRule[T]) = rab(at.t)(rr).log(at.msg)
    }
  }

  implicit def rewriteAtLens[S, T]: RewriteAtBuilder[Lens[S, T], S, T] = new RewriteAtBuilder[Lens[S, T], S, T] {
    override def apply(at: Lens[S, T]) = new RewriteAt[S, T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (s: S) =>
        val lsets = at.set(_: T)(s)
        for {
          rrs <- rr(at.get(s))
        } yield rrs.bimap(
          lsets,
          _ map lsets
        )
      }
    }
  }

  implicit def rewriteAtPrism[S, T]: RewriteAtBuilder[Prism[S, T], S, T] = new RewriteAtBuilder[Prism[S, T], S, T] {
    override def apply(at: Prism[S, T]) = new RewriteAt[S, T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (s: S) =>
        val psets = at.set(_: T)(s)
        at.getOrModify(s).fold(
          _.left[Rewritten[S]].point[EvalState] : MaybeRewritten[S],
          r => for {
            rrr <- rr(r)
          } yield rrr.fold(
            l => psets(l).left[Rewritten[S]],
            r => (r map psets).right[S])
        )  : MaybeRewritten[S]
      }
    }
  }

  implicit def filterRewrite[T]: RewriteAtBuilder[T => Boolean, T, T] = new RewriteAtBuilder[T => Boolean, T, T] {
    override def apply(at: (T) => Boolean) = new RewriteAt[T, T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (t: T) =>
        if(at(t)) rr(t)
        else t.left[Rewritten[T]].point[EvalState]
      }
    }
  }

  implicit def rewriteAtFilteringEq[S, T]: RewriteAtBuilder[RewriteRule.FilteringEq[S, T], S, S] = new RewriteAtBuilder[RewriteRule.FilteringEq[S, T], S, S] {
    override def apply(at: FilteringEq[S, T]) = filterRewrite[S] { (s: S) =>
      at.l.get(s) == at.t
    }
  }

  implicit def rewriteAtAllElements[T]: RewriteAtBuilder[RewriteRule.allElements.type, List[T], T] = new RewriteAtBuilder[RewriteRule.allElements.type, List[T], T] {

  override def apply(at: RewriteRule.allElements.type) = new RewriteAt[List[T], T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (ts: List[T]) => for {
        rrts <- (ts map rr.apply).sequenceU
      } yield
        if (rrts exists (_.isRight)) {
          (rrts collect {
            case -\/(l) => l.point[Rewritten]
            case \/-(r) => r
          }).sequenceU.right[List[T]]
        } else {
          (rrts collect { case -\/(l) => l }).left[Rewritten[List[T]]]
        }
      }
    }
  }

  implicit def rewriteAtPair[F, G, S, T](implicit rwF: RewriteAtBuilder[F, S, T], rwG: RewriteAtBuilder[G, S, T]):
  RewriteAtBuilder[(F, G), S, T] = new RewriteAtBuilder[(F, G), S, T] {
    override def apply(pair: (F, G)) = new RewriteAt[S, T] {
      override def apply(rr: RewriteRule[T]) = (rr at pair._1) andThen (rr at pair._2)
    }
  }


  import longhandAst._
  import optics.{longhand => ol}
  import RewriteRule.{FilteringOps, OfType}

  implicit def rewriteAtOfType[I](implicit iToIdentifier: I => Identifier):
  RewriteAtBuilder[OfType[I], ConstructorApp, List[PropertyExp]] =
    new RewriteAtBuilder[OfType[I], ConstructorApp, List[PropertyExp]] {
      override def apply(ofT: OfType[I]) = new RewriteAt[ConstructorApp, List[PropertyExp]] {
        override def apply(rr: RewriteRule[List[PropertyExp]]) = rr at
          ol.ConstructorApp.body at
          ((ol.ConstructorApp.cstr composeLens ol.TpeConstructor.tpe) :== iToIdentifier(ofT.i))
      }
    }

  implicit def rewriteAtPropertyToLiteral[I](implicit iToIdentifier: I => Identifier):
  RewriteAtBuilder[I, List[PropertyExp], Literal] =
    new RewriteAtBuilder[I, List[PropertyExp], Literal] {
      override def apply(prop: I) = new RewriteAt[List[PropertyExp], Literal] {
        override def apply(rr: RewriteRule[Literal]) = rr at
          ol.PropertyValue.Literal.value at
          ol.PropertyValue.asLiteral at
          ol.PropertyExp.value at
          (ol.PropertyExp.property :== prop) at
          RewriteRule.allElements
      }
    }

  implicit val rewriteAtStarToLiteral:
  RewriteAtBuilder[RewriteRule.*.type, List[PropertyExp], Literal] =
    new RewriteAtBuilder[RewriteRule.*.type, List[PropertyExp], Literal] {
      override def apply(star: RewriteRule.*.type) = new RewriteAt[List[PropertyExp], Literal] {
        override def apply(rr: RewriteRule[Literal]) = rr at
          ol.PropertyValue.Literal.value at
          ol.PropertyValue.asLiteral at
          ol.PropertyExp.value at
          RewriteRule.allElements
      }
    }

  implicit def rewriteAtPropertyToReference[I](implicit iToIdentifier: I => Identifier):
  RewriteAtBuilder[I, List[PropertyExp], PropertyValue.Reference] =
    new RewriteAtBuilder[I, List[PropertyExp], PropertyValue.Reference] {
      override def apply(prop: I) = new RewriteAt[List[PropertyExp], PropertyValue.Reference] {
        override def apply(rr: RewriteRule[PropertyValue.Reference]) = rr at
          ol.PropertyValue.asReference at
          ol.PropertyExp.value at
          (ol.PropertyExp.property :== prop) at
          RewriteRule.allElements
      }
    }

  implicit val rewriteAtStarToReference:
  RewriteAtBuilder[RewriteRule.*.type, List[PropertyExp], PropertyValue.Reference] =
    new RewriteAtBuilder[RewriteRule.*.type, List[PropertyExp], PropertyValue.Reference] {
      override def apply(star: RewriteRule.*.type) = new RewriteAt[List[PropertyExp], PropertyValue.Reference] {
        override def apply(rr: RewriteRule[PropertyValue.Reference]) = rr at
          ol.PropertyValue.asReference at
          ol.PropertyExp.value at
          RewriteRule.allElements
      }
    }

  implicit def rewriteAtPropertyToValue[I](implicit iToIdentifier: I => Identifier):
  RewriteAtBuilder[I, List[PropertyExp], PropertyValue] =
    new RewriteAtBuilder[I, List[PropertyExp], PropertyValue] {
      override def apply(prop: I) = new RewriteAt[List[PropertyExp], PropertyValue] {
        override def apply(rr: RewriteRule[PropertyValue]) = rr at
          ol.PropertyExp.value at
          (ol.PropertyExp.property :== prop) at
          RewriteRule.allElements
      }
    }

  implicit def rewriteAtPropertyToProperties[I](implicit iToIdentifier: I => Identifier):
  RewriteAtBuilder[I, List[PropertyExp], List[PropertyExp]] =
    new RewriteAtBuilder[I, List[PropertyExp], List[PropertyExp]] {
      override def apply(prop: I) = new RewriteAt[List[PropertyExp], List[PropertyExp]] {
        override def apply(rr: RewriteRule[List[PropertyExp]]) = rr at
          ol.ConstructorApp.body at
          ol.PropertyValue.Nested.value at
          ol.PropertyValue.asNested at
          ol.PropertyExp.value at
          (ol.PropertyExp.property :== prop) at
          RewriteRule.allElements
      }
    }
}

