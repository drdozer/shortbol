package uk.co.turingatemyhamster.shortbol
package ops

import ast._
import ast.sugar._
import monocle._
import Monocle._
import uk.co.turingatemyhamster.shortbol.ast.{ConstructorApp, ValueExp}
import scalaz._
import Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
//object SBOL extends ConstraintSystem {
//  val rdf_about = "rdf" :# "about"
//  val sbol_TopLevel = "sbol" :# "TopLevel"
//  val sbol_Identified = "sbol" :# "Identified"
//
//  val topCstr = optics.topLevel.instanceExp.instanceExp composeLens
//    optics.instanceExp.cstrApp
//
//  val nestedCstrs =
//    ('body, optics.constructorApp.body) @:
//    ('property, each[List[BodyStmt], BodyStmt] composeOptional
//      optics.bodyStmt.property composeLens
//      second) @:
//    ('instance, stdRight[ValueExp, ConstructorApp]) @:
//    (_: Constraint[ConstructorApp])
//
//  val noRdfAboutExists =
//    (rdf_about,
//      each[List[BodyStmt], BodyStmt] composeOptional optics.bodyStmt.propValue(rdf_about) ) @:
//      ('size, Getter((_: List[optics.bodyStmt.PropValue]).length)) @:
//      NotLessThan(1)
//
//
//  class ContextConstraint(ctxt: EvalContext) extends Constraint[TopLevel.InstanceExp] {
//    val typer = OwlTyper(ctxt)
//
//    val instanceOfTopLevel = typer.byType[ConstructorApp](sbol_TopLevel)
//    val instanceOfIdentified = typer.byType[ConstructorApp](sbol_Identified)
//    val rdfAboutMissingInBody = ('body, optics.constructorApp.body) @: noRdfAboutExists
//    val topLevelNotEmbedded = instanceOfTopLevel.not
//
//
//    def checkAndRecurse(c: Constraint[ConstructorApp]): Constraint[ConstructorApp] = If(
//      instanceOfIdentified,
//      Constraint.applyAll(
//        List(
//          c,
//          nested(checkAndRecurse(c))
//        )
//      ),
//      Constraint.success)
//
//    def nested(c: Constraint[ConstructorApp]) = nestedCstrs apply c
//
//    val topConstraint = ('topCstr, topCstr) @:
//      (nested(checkAndRecurse(Constraint.applyAll(List(topLevelNotEmbedded, rdfAboutMissingInBody)))) onlyIf instanceOfTopLevel)
//
//    override def apply(a: TopLevel.InstanceExp) = topConstraint apply a
//
//    override def not = ???
//
//    override def prettyPrint = topConstraint.prettyPrint
//  }
//
//  override def fromContext(ctxt: EvalContext) = new ContextConstraint(ctxt)
//}
//
//object SBOLRecovery {
//
//  def addTopLevel(ie: InstanceExp) = ValueRecovery[SBEvaluatedFile] {
//    sef =>
//      (Scalaz.none -> sef.copy(tops = sef.tops :+ TopLevel.InstanceExp(ie))).some
//  }
//
//  def nestedTopLevelRecovery(mkAbout: () => Identifier) = ConstraintRecovery.nested[optics.bodyStmt.PropValue, Symbol, ConstructorApp] {
//    case NestedViolation(pv, 'instance, caV) =>
//      caV match {
//        case NestedViolation(ca, 'type, tV) =>
//          tV.asInstanceOf[ConstraintFailure[Set[Identifier]]] match {
//            case ConstraintFailure(nmo, _) =>
//              nmo match {
//                case NotMemberOf(_) =>
//                  val newId = mkAbout() : Identifier
//                  (addTopLevel(InstanceExp(newId, ca)).some, Left(newId: ValueExp)).some
//                case _ =>
//                  Scalaz.none
//              }
//            case _ =>
//              Scalaz.none
//          }
//        case _ =>
//          Scalaz.none
//      }
//    case _ =>
//      Scalaz.none
//  }
//
//  def nestedAboutRecovery(mkAbout: () => Identifier) = ConstraintRecovery.nested[ConstructorApp, Symbol, optics.bodyStmt.PropValue] {
//    case NestedViolation(cApp, 'body, bV) =>
//      bV match {
//        case NestedViolation(body, SBOL.`rdf_about`, sV) =>
//          sV match {
//            case NestedViolation(_, 'size, cF) =>
//              cF.asInstanceOf[ConstraintFailure[Int]] match {
//                case ConstraintFailure(NotLessThan(1), _) =>
//                  (Scalaz.none ->
//                    cApp.copy(body = (Assignment(SBOL.rdf_about, mkAbout()) : BodyStmt) +: cApp.body)).some
//                case _ =>
//                  Scalaz.none
//              }
//            case _ =>
//              Scalaz.none
//          }
//        case _ =>
//          Scalaz.none
//      }
//    case _ =>
//      Scalaz.none
//  }
//
//}