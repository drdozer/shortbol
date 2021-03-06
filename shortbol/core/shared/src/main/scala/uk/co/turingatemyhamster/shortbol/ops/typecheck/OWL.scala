package uk.co.turingatemyhamster.shortbol
package ops
package typecheck

import monocle.Monocle.{none => _}
import longhandAst._
import sharedAst.{Identifier, IntegerLiteral, Literal, StringLiteral}
import sharedAst.sugar._
import uk.co.turingatemyhamster.shortbol.longhandAst.InstanceExp


trait Typer[A] {
  def exactTypeOf(a: A): Set[Identifier]
}

object Typer {

  implicit val instanceExpTyper: Typer[InstanceExp] = new Typer[InstanceExp] {
    override def exactTypeOf(a: InstanceExp) = implicitly[Typer[ConstructorApp]].exactTypeOf(a.cstrApp)
  }

  implicit val constructorAppTyper: Typer[ConstructorApp] = new Typer[ConstructorApp] {
    override def exactTypeOf(a: ConstructorApp) = a match {
      case ConstructorApp(TpeConstructor(t), _) => Set(t)
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

case class OwlTyper(ctxt: EvalContext) {

  val allClassIds = for {
        is <- ctxt.insts.values.to[List]
        _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.identifier}")
        InstanceExp(clsId, ConstructorApp(TpeConstructor(terms.OWL.`class`), _)) <- is
  } yield clsId

  val classHierarchy = (for {
    is <- ctxt.insts.values.to[List]
    _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.identifier}")
    InstanceExp(clsId, ConstructorApp(TpeConstructor(terms.OWL.`class`), clsBdy)) <- is
    PropertyExp(terms.OWL.`subClassOf`, PropertyValue.Reference(superType)) <- clsBdy
  } yield clsId -> superType)
    .foldLeft(Map.empty[Identifier, List[Identifier]])((m, ii) => m + (ii._1 -> (ii._2 :: m.getOrElse(ii._1, Nil))))

  val flatHierarchy = {
    val cache = scala.collection.mutable.Map.empty[Identifier, Set[Identifier]]

    def unwrap(i: Identifier): Set[Identifier] = cache.getOrElseUpdate(
      i,
      Set(i) ++ (classHierarchy.getOrElse(i, Nil) flatMap unwrap))

    allClassIds foreach unwrap

    Map(cache.to[Seq] :_*)
  }

  def byType[A](tpe: Identifier)(implicit ty: Typer[A]) =
    ('type,
      (a: A) =>
        ty exactTypeOf a flatMap (ta =>
          flatHierarchy.getOrElse(ta, Set(ta)))
    ) @: SetContainsMember(tpe)

}



/**
  *
  *
  * @author Matthew Pocock
  */
object OWL extends ConstraintSystem {

  /// fixme: Make a sane type that catalogues the various validations
  type Report = Nothing

  override def validate(ctxt: EvalContext): (InstanceExp) => Nothing = (ie: InstanceExp) => ???

  def bySize[T](c: Constraint[Int]) = ('size, (_: List[T]).size) @: c

  def minCardinalityConstraint(propId: Identifier, propRes: PropertyExp): Option[Constraint[List[PropertyExp]]] =
    propRes match {
      case PropertyExp(terms.OWL.minCardinality, PropertyValue.Literal(IntegerLiteral(c))) =>
        Some(propId @: bySize(NotLessThan(c)))
      case _ =>
        None
    }

  def maxCardinalityConstraint(propId: Identifier, propRes: PropertyExp): Option[Constraint[List[PropertyExp]]] =
    propRes match {
      case PropertyExp(terms.OWL.maxCardinality, PropertyValue.Literal(IntegerLiteral(c))) =>
        Some(propId @: bySize(NotGreaterThan(c)))
      case _ =>
        None
    }

  def exactCardinalityConstraint(propId: Identifier, propRes: PropertyExp): Option[Constraint[List[PropertyExp]]] =
    propRes match {
      case PropertyExp(terms.OWL.exactCardinality, PropertyValue.Literal(IntegerLiteral(c))) =>
        Some(propId @: bySize(NotLessThan(c) && NotGreaterThan(c)))
      case _ =>
        None
    }

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
}
