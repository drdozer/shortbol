package uk.co.turingatemyhamster.shortbol
package ops

import scalaz.Scalaz._
import scalaz._
import monocle.{Lens, Prism}
import RewriteRule.{FilteringEq, MaybeRewritten, Rewritten}
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.shorthandAst.{Identifier, Literal}

import scala.annotation.implicitNotFound

/**
  * Created by nmrp3 on 09/11/16.
  */
trait RewriteRule[T] {
  self =>

  def apply(t: T): MaybeRewritten[T]

  def eval(t: T): Rewritten[T] = apply(t).fold(_.point[Rewritten], identity)

  def at[AT, U](at: AT)(implicit rwAt: RewriteAtBuilder[AT, U, T]): RewriteRule[U] = rwAt(at)(this)

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
      println(s"$name <o> $descriptionString ${res.fold(_ => "unchanged", _ => "rewritten")} at $t")
      res
    }
  }
}

object RewriteRule {
  implicit class FilteringOps[S, T](_l: Lens[S, T]) {
    def :==(t: T) = FilteringEq[S, T](_l, t)
  }

  case class FilteringEq[S, T](l: Lens[S, T], t: T)

  def ofType[I](i: I) = OfType(i)

  case class OfType[I](i: I)

  type MaybeRewritten[T] = T \/ Rewritten[T]
  type InstanceExpWriter[T] = Writer[List[longhandAst.InstanceExp], T]
  type Rewritten[T] = Eval.EvalStateT[InstanceExpWriter, T]

  def Rewritten[T](w: InstanceExpWriter[T]): Rewritten[T] =
    IndexedStateT[InstanceExpWriter, EvalContext, EvalContext, T](s => w.map(s -> _))

  def rewrite(r: RewriteRule[longhandAst.SBFile], sf: longhandAst.SBFile): EvalState[longhandAst.SBFile] = {
    var depth = 0
    def rewriteStep(sf: longhandAst.SBFile): EvalState[longhandAst.SBFile] = {
      if(depth > 3) throw new IllegalStateException("Recursed too deeply during rewrite rules. Perhaps a rewrite rule is misfiring?")
      depth += 1
      r(sf).fold(
        _.point[EvalState],
        rsf =>
          for {
            s <- get[EvalContext]
            (extraIs, (newC, newSf)) = rsf.run(s).run
            r <- rewriteStep(longhandAst.SBFile(newSf.tops ::: extraIs))
          } yield r
      )
    }
    rewriteStep(sf)
  }

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

  implicit def rewriteAtLens[S, T]: RewriteAtBuilder[Lens[S, T], S, T] = new RewriteAtBuilder[Lens[S, T], S, T] {
    override def apply(at: Lens[S, T]) = new RewriteAt[S, T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (s: S) =>
        val lsets = at.set(_: T)(s)
        rr(at.get(s)).bimap(
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
        at.getOrModify(s) flatMap {
          rr apply _ bimap(
            psets,
            _ map psets
          )
        }
      }
    }
  }

  implicit def filterRewrite[T]: RewriteAtBuilder[T => Boolean, T, T] = new RewriteAtBuilder[T => Boolean, T, T] {
    override def apply(at: (T) => Boolean) = new RewriteAt[T, T] {
      override def apply(rr: RewriteRule[T]) = RewriteRule { (t: T) =>
        if(at(t)) rr(t)
        else -\/(t)
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
      override def apply(rr: RewriteRule[T]) = RewriteRule { (ts: List[T]) =>
        val rrts = ts map rr.apply
        if (rrts exists (_.isRight)) {
          (rrts collect {
            case -\/(l) => l.point[Rewritten]
            case \/-(r) => r
          }).sequenceU.right[List[T]]: RewriteRule.MaybeRewritten[List[T]]
        } else {
          (rrts collect { case -\/(l) => l }).left: RewriteRule.MaybeRewritten[List[T]]
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

