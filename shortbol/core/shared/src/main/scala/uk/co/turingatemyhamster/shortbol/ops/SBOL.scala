package uk.co.turingatemyhamster.shortbol
package ops

import ast._
import ast.sugar._
import monocle._
import Monocle._
import uk.co.turingatemyhamster.shortbol.ast.TopLevel.InstanceExp
import uk.co.turingatemyhamster.shortbol.ast.{ConstructorApp, ValueExp}

/**
  *
  *
  * @author Matthew Pocock
  */
object SBOL extends ConstraintSystem {
  val rdf_about = "rdf" :# "about"
  val sbol_TopLevel = "sbol" :# "TopLevel"
  val sbol_Identified = "sbol" :# "Identified"

  val topCstr = optics.topLevel.instanceExp.instanceExp composeLens
    optics.instanceExp.cstrApp

  val nestedCstrs = optics.constructorApp.body composeTraversal
        each composeOptional
          optics.bodyStmt.property composeLens
            second composePrism
              stdRight

  val noRdfAboutExists =
    (rdf_about,
      each[List[BodyStmt], BodyStmt] composeOptional optics.bodyStmt.propValue(rdf_about) ) @:
      ('size, Getter((_: List[optics.bodyStmt.PropValue]).length)) @:
      NotLessThan(1)

  class ContextConstraint(ctxt: EvalContext) extends Constraint[TopLevel.InstanceExp] {
    val typer = OwlTyper(ctxt)

    lazy val instanceOfTopLevel = typer.byType[ConstructorApp](sbol_TopLevel)
    lazy val instanceOfIdentified = typer.byType[ConstructorApp](sbol_Identified)

    lazy val checkAndRecurse: Constraint[ConstructorApp] = If(
      instanceOfIdentified,
      Constraint.applyAll(
        List(
          ('body, optics.constructorApp.body) @: noRdfAboutExists,
          checkNestedCstrs
        )
      ),
      Constraint.success)

    lazy val checkNestedCstrs: Constraint[ConstructorApp] =
      ('nestedCstr, nestedCstrs) @: checkAndRecurse

    val topConstraint = ('topCstr, topCstr) @: (checkNestedCstrs onlyIf instanceOfTopLevel)

    override def apply(a: InstanceExp) = topConstraint apply a

    override def prettyPrint = topConstraint.prettyPrint
  }

  override def fromContext(ctxt: EvalContext) = new ContextConstraint(ctxt)
}
