package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import monocle.Monocle._
import shorthandAst.sugar._
import longhandAst.sugar._
import RewriteRule.{ofType, allElements}
import optics.{longhand => ol}
import ol.SBFile._
import ol.InstanceExp._
import longhandAst.{PropertyExp, PropertyValue}
import shorthandAst.Identifier
import terms.RDF
import terms.SBOL._
import PropertyStep.PropertyStepOps

/**
  * Created by nmrp3 on 17/11/16.
  */
object RepairModule {

  lazy val repairParticipants = RewriteRule { (ps: List[PropertyExp]) =>
    val defToAbout = (for {
      cds <- ps collect { case PropertyExp(`functionalComponent`, PropertyValue.Nested(ca)) => ca }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
      about <- cds.body collect { case PropertyExp(RDF.about, PropertyValue.Reference(r)) => r }
    } yield (defnt, about)).toMap

    RewriteRule { (ref: PropertyValue.Reference) =>
      defToAbout get ref.value map PropertyValue.Reference
    } at (interaction --> participation --> participant)
  }


  lazy val componentsForRefs = RepairOps build { (ref: Identifier) =>
    println("Repairing component for ref $ref")
    functionalComponent := FunctionalComponent(
      access := access_public,
      definition := ref,
      direction := inout
    )
  } from
    (interaction --> participation --> participant) excluding
    (functionalComponent --> (definition, RDF.about))


  lazy val replaceComponentReferenceWithComponent = RepairOps.replaceReference { (ref: Identifier, lnO: Option[String]) =>
    PropertyValue.Nested(
      FunctionalComponent(
        displayId := lnO map slLit,
        access := access_public,
        definition := ref,
        direction := inout)) : PropertyValue
  }


  lazy val repairAtComponent = replaceComponentReferenceWithComponent at
    functionalComponent

  lazy val replaceModuleReferenceWithModule = RepairOps.replaceReference { (ref: Identifier, lnO: Option[String]) =>
    PropertyValue.Nested(
      Module(
        displayId := lnO map slLit,
        definition := ref
      )
    ) : PropertyValue
  }

  lazy val repairAtModule = replaceModuleReferenceWithModule at
    module

  lazy val repairAtModuleDefinition =
    (
      repairAtComponent andThen
        componentsForRefs andThen
        repairParticipants andThen
        repairAtModule
      ) at
      ofType(ModuleDefinition)

  lazy val repairAll = repairAtModuleDefinition at
    cstrApp at
    allElements at
    tops
}
