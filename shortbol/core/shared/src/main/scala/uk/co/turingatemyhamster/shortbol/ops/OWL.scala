package uk.co.turingatemyhamster.shortbol
package ops

import scala.reflect.runtime.universe.TypeTag
import monocle.{Monocle, Getter}
import Monocle.{none => _, _}

import ast._
import ast.sugar._

import scalaz.Scalaz._


trait Typer[A] {
  def exactTypeOf(a: A): Set[Identifier]
}

object Typer {

  implicit val instanceExpTyper: Typer[InstanceExp] = new Typer[InstanceExp] {
    override def exactTypeOf(a: InstanceExp) = implicitly[Typer[ConstructorApp]].exactTypeOf(a.cstrApp)
  }

  implicit val constructorAppTyper: Typer[ConstructorApp] = new Typer[ConstructorApp] {
    override def exactTypeOf(a: ConstructorApp) = a match {
      case ConstructorApp(TpeConstructor1(t, _), _) => Set(t)
      case _ => Set()
    }
  }

  implicit val literalTyper: Typer[Literal] = new Typer[Literal] {
    override def exactTypeOf(a: Literal) = a match {
      case IntegerLiteral(_) => Set("xsd" :# "integer")
      case StringLiteral(_, dt, _) => dt map (d => Set(d.tpe)) getOrElse Set("xsd" :# "string")
    }
  }

}
//
//case class OwlTyper(ctxt: EvalContext) {
//
//  val allClassIds = for {
//        is <- ctxt.insts.values.to[List]
//        _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
//        InstanceExp(clsId, ConstructorApp(TpeConstructor1(OWL.`owl_class`, _), _)) <- is
//  } yield clsId
//
//  val classHierarchy = (for {
//    is <- ctxt.insts.values.to[List]
//    _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
//    InstanceExp(clsId, ConstructorApp(TpeConstructor1(OWL.`owl_class`, _), clsBdy)) <- is
//    BodyStmt.Assignment(Assignment(OWL.`owl_subClassOf`, ValueExp.Identifier(superType))) <- clsBdy
//  } yield clsId -> superType)
//    .foldLeft(Map.empty[Identifier, List[Identifier]])((m, ii) => m + (ii._1 -> (ii._2 :: m.getOrElse(ii._1, Nil))))
//
//  val flatHierarchy = {
//    val cache = scala.collection.mutable.Map.empty[Identifier, Set[Identifier]]
//
//    def unwrap(i: Identifier): Set[Identifier] = cache.getOrElseUpdate(
//      i,
//      Set(i) ++ (classHierarchy.getOrElse(i, Nil) flatMap unwrap))
//
//    allClassIds foreach unwrap
//
//    Map(cache.to[Seq] :_*)
//  }
//
//  def byType[A](tpe: Identifier)(implicit ty: Typer[A], aTpe: TypeTag[A]) =
//    ('type, Getter((a: A) => {
//      ty exactTypeOf a flatMap (ta =>
//        flatHierarchy.getOrElse(ta, Set(ta)))
//    })) @: MemberOf(tpe)
//
//}
//
//
//
///**
//  *
//  *
//  * @author Matthew Pocock
//  */
//object OWL extends ConstraintSystem {
//  val owl = "owl": NSPrefix
//  val owl_class = owl :# "Class"
//  val owl_propertyRestriction = owl :# "propertyRestriction"
//  val owl_minCardinality = owl :# "minCardinality"
//  val owl_maxCardinality = owl :# "maxCardinality"
//  val owl_exactCardinality = owl :# "exactCardinality"
//  val owl_allValuesFrom = owl :# "allValuesFrom"
//  val owl_subClassOf = owl :# "subClassOf"
//
//  def bySize[T](c: Constraint[Int])(implicit tTpe: TypeTag[T]) = ('size, Getter((_: List[T]).size)) @: c
//
//  def minCardinalityConstraint(propId: Identifier, propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
//    propRes match {
//      case BodyStmt.Assignment(Assignment(`owl_minCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
//        ((propId,
//          each[List[BodyStmt], BodyStmt] composeOptional optics.bodyStmt.propValue(propId) ) @:
//          ('size, Getter((_: List[optics.bodyStmt.PropValue]).length)) @:
//            NotLessThan(c)).some
//      case _ =>
//        none
//    }
//
//  def maxCardinalityConstraint(propId: Identifier, propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
//    propRes match {
//      case BodyStmt.Assignment(Assignment(`owl_maxCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
//        ((propId,
//          each[List[BodyStmt], BodyStmt] composeOptional optics.bodyStmt.propValue(propId) ) @:
//          ('size, Getter((_: List[optics.bodyStmt.PropValue]).length)) @:
//            NotGreaterThan(c)).some
//      case _ =>
//        none
//    }
//
//  def exactCardinalityConstraint(propId: Identifier, propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
//    propRes match {
//      case BodyStmt.Assignment(Assignment(`owl_exactCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
//        ((propId,
//          each[List[BodyStmt], BodyStmt] composeOptional optics.bodyStmt.propValue(propId) ) @:
//          ('size, Getter((_: List[optics.bodyStmt.PropValue]).length)) @:
//          Constraint.applyAll(NotLessThan(c) :: NotGreaterThan(c) :: Nil)).some
//      case _ =>
//        none
//    }
//
//
//  class ContextConstraints(ctxt: EvalContext) extends Constraint[TopLevel.InstanceExp] {
//    val typer = OwlTyper(ctxt)
//
//
//    def allValuesFromConstraint(propId: Identifier, propRes: BodyStmt): Option[Constraint[BodyStmt]] =
//      propRes match {
//        case BodyStmt.Assignment(Assignment(`owl_allValuesFrom`, ValueExp.Identifier(tpe))) =>
//          (
//            (propId, optics.bodyStmt.propValue(propId)) @:
//              Constraint.applyAll(
//                List(
//                  ('assignment, stdLeft[ValueExp, ConstructorApp])
//                    @: Constraint.applyAll(
//                    List(
//                      ('identifier, optics.valueExp.identifier) @:
//                        ('resolveIdentifier, Getter { (i: Identifier) =>
//                          for {
//                            eqA <- (equIds.getOrElse(i, Set(i))).to[List]
//                            ie <- ctxt.insts get eqA flatMap (_.headOption)
//                          } yield {
//                            println(s"resolved $i to $ie")
//                            ie
//                          }
//                        }
//                        ) @: Constraint.forAny(typer.byType[InstanceExp](tpe) ),
//                      ('literal, optics.valueExp.literal) @: typer.byType[Literal](tpe)
//                    )
//                  ),
//                  ('instance, stdRight[ValueExp, ConstructorApp])
//                    @: typer.byType[ConstructorApp](tpe)
//                )
//              )
//            ).some
//        case _ =>
//          none
//      }
//
//    def restrictions(propId: Identifier, restrs: Seq[BodyStmt]): Constraint[List[BodyStmt]] =
//      Constraint.applyAll(
//        restrs.to[List] flatMap { r =>
//          List.empty ++
//            minCardinalityConstraint(propId, r) ++
//            maxCardinalityConstraint(propId, r) ++
//            exactCardinalityConstraint(propId, r) ++
//            (allValuesFromConstraint(propId, r).to[List] map Constraint.forEvery)
//        }
//      )
//
//    def restrictionInstance(i: InstanceExp): Option[Constraint[List[BodyStmt]]] = i match {
//      case InstanceExp(propId, ConstructorApp(TpeConstructor1(`owl_propertyRestriction`, _), restrs)) =>
//        restrictions(propId, restrs).some
//      case _ => None
//    }
//
//    def owlClassConstraint(cl: InstanceExp): Option[Constraint[InstanceExp]] =
//      cl match {
//        case InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), clsBdy)) =>
//          val rs = Constraint.applyAll(
//            clsBdy.to[List].flatMap {
//              case BodyStmt.InstanceExp(i) => restrictionInstance(i)
//              case _ => Nil
//            })
//          ((('properties, optics.instanceExp.cstrApp composeLens optics.constructorApp.body) @: rs) onlyIf
//            typer.byType[InstanceExp](clsId)).some
//        case _ =>
//          none
//      }
//
//    val equIds = {
//      var clusters = Map.empty[Identifier, Set[Identifier]]
//
//      for {
//        (i, js) <- ctxt.vlxps
//        _ = if(js.size != 1) throw new IllegalStateException(s"Expected one value for $i but found $js")
//        ValueExp.Identifier(j) <- js
//      } {
//        val iS = clusters.getOrElse(i, Set(i))
//        val jS = clusters.getOrElse(j, Set(j))
//        val ijS = iS ++ jS
//        clusters += (i -> ijS)
//        clusters += (j -> ijS)
//      }
//
//      clusters
//    }
//
//    val allOwlClasses = (for {
//      is <- ctxt.insts.values.to[List]
//      _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
//      i <- is
//      c <- owlClassConstraint(i)
//    } yield i.id -> c).toMap
//
//    val flatClasses = typer.flatHierarchy.values map (tps => Constraint.applyAll(tps.to[List] map allOwlClasses))
//
//    val tcInstanceExp = Constraint.applyAll(flatClasses.to[List])
//
//    val tcTopLevel = ('instanceExp, optics.topLevel.instanceExp.instanceExp) @: tcInstanceExp
//
//    override def apply(a: TopLevel.InstanceExp) =
//      tcTopLevel apply a
//
//    override def not = ???
//
//    override def prettyPrint: String = tcTopLevel.prettyPrint
//  }
//
//  override def fromContext(ctxt: EvalContext): ContextConstraints = new ContextConstraints(ctxt)
//}
