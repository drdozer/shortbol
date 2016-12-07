package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import longhandAst.{ConstructorApp, PropertyExp, PropertyValue}
import optics.longhand.PropertyValue.asReference
import optics.longhand.PropertyValue.Reference.{value => referenceValue}
import sharedAst.{Identifier, LocalName, QName}

import scala.annotation.implicitNotFound

/**
  * Created by nmrp3 on 24/11/16.
  */
object RepairOps {
  private val reference = asReference composeLens referenceValue
  def replaceReference(generate: (Identifier, Option[String]) => PropertyValue) = RewriteRule { (pv: PropertyValue) =>
    for {
      ref <- reference getOrModify pv
    } yield {
      val lnO = ref match {
        case LocalName(ln) => Some(ln)
        case QName(_, LocalName(ln)) => Some(ln)
        case _ => None
      }

      generate(ref, lnO)
    }
  }

  def deReference[T](t: T)(implicit tp: ReferencePath[T]) = new {
    def in(pes: List[PropertyExp]): List[Identifier] = pes flatMap in
    def in(pe: PropertyExp): List[Identifier] = tp(t, pe)
  }

  def build[PE](builder: Identifier => List[PE])(implicit pe: PE => PropertyExp): FromBuilder = new FromBuilder {
    override def from[F](f: F)(implicit ppF: ReferencePath[F]) = new ExcludingBuilder {
      override def excluding[E](e: E)(implicit ppE: ReferencePath[E]) = RewriteRule { (ps: List[PropertyExp]) =>
        (ps.to[Set].flatMap(ppF(f, _)) -- ps.flatMap(ppE(e, _))).to[List] flatMap builder map pe match {
          case Nil => None
          case orphans => Some(orphans ::: ps)
        }
      }
    }
  }

  trait FromBuilder {
    def from[F](f: F)(implicit ppF: ReferencePath[F]): ExcludingBuilder
  }

  trait ExcludingBuilder {
    def excluding[E](e: E)(implicit ppE: ReferencePath[E]): RewriteRule[List[PropertyExp]]
  }

  @implicitNotFound("Could not build a ReferencePath for ${T}")
  trait ReferencePath[T] {
    def apply(t: T, pe: PropertyExp): List[Identifier]
  }

  @implicitNotFound("coud not build a ConstructorAppPath for ${T}")
  trait ConstructorAppPath[T] {
    def apply(t: T, pe: PropertyExp): List[ConstructorApp]
  }

  implicit def pairPath[L, R](implicit pL: ReferencePath[L], pR: ReferencePath[R]): ReferencePath[(L, R)] = new ReferencePath[(L, R)] {
    override def apply(t: (L, R), pe: PropertyExp) =
      pL(t._1, pe) ++
        pR(t._2, pe)
  }


  implicit def identifierConstructorAppPath[I](implicit ie: I => Identifier): ConstructorAppPath[I] = new ConstructorAppPath[I] {
    override def apply(t: I, pe: PropertyExp): List[ConstructorApp] = pe match {
      case PropertyExp(p, PropertyValue.Nested(c)) if p == ie(t) => c :: Nil
      case _ => Nil
    }
  }

  implicit def identifierReferencePath[I](implicit ie: I => Identifier): ReferencePath[I] = new ReferencePath[I] {
    override def apply(t: I, pe: PropertyExp): List[Identifier] = pe match {
      case PropertyExp(p, PropertyValue.Reference(r)) if p == ie(t) => r :: Nil
      case _ => Nil
    }
  }

}
