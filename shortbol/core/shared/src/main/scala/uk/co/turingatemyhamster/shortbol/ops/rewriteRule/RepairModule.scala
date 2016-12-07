package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import scalaz._
import Scalaz._
import monocle.Monocle._
import shorthandAst.sugar._
import longhandAst.sugar._
import RewriteRule.{allElements, ofType}
import optics.{longhand => ol}
import ol.SBFile._
import ol.InstanceExp._
import longhandAst.{InstanceExp, PropertyExp, PropertyValue}
import shorthandAst.Identifier
import terms.RDF
import terms.SBOL._
import PropertyStep.PropertyStepOps

case class RepairMapsTo(componentProperty: Identifier) {

  def defToAbout(ps: List[PropertyExp]): Map[Identifier, Identifier] = (for {
    cds <- ps collect { case PropertyExp(`componentProperty`, PropertyValue.Nested(ca)) => ca }
    defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
    about <- cds.body collect { case PropertyExp(RDF.about, PropertyValue.Reference(r)) => r }
  } yield (defnt, about)).toMap

  lazy val repairReferences = new {
    def at [L](loc: L)(implicit locRR: RewriteAtBuilder[L, List[PropertyExp], PropertyValue.Reference]) =
    RewriteRule { (ps: List[PropertyExp]) =>
      val d2a = defToAbout(ps)

      RewriteRule { (ref: PropertyValue.Reference) =>
        d2a get ref.value map PropertyValue.Reference
      } at loc
    }
  }

  lazy val repairMapsToRemote = RewriteRule { (ps: List[PropertyExp]) =>
    for {
      remoteRef <- (RepairOps deReference definition in ps).point[Eval.EvalState]
      remoteMDs <- remoteRef.traverseU(r => Eval.inst(r).map(_.to[List]))
    } yield {
      val d2a = remoteMDs.flatten.map(i => defToAbout(i.cstrApp.body)).foldLeft(Map.empty[Identifier, Identifier])(_ ++ _)

      RewriteRule { (ref: PropertyValue.Reference) =>
        d2a get ref.value map PropertyValue.Reference
      } at (mapsTo --> remote)
    }
  }
}

/**
  * Created by nmrp3 on 17/11/16.
  */
object RepairModule extends InstanceRewriter {

  lazy val repairMapsTo = RepairMapsTo(functionalComponent)

  lazy val repairParticipantsAndMapsToLocal = repairMapsTo.repairReferences at (
    interaction --> participation --> participant,
    module --> mapsTo --> local
  )

  lazy val repairModuleMapsToRemote = repairMapsTo.repairMapsToRemote at module

  lazy val repairFunctionalComponentMapsToRemote = repairMapsTo.repairMapsToRemote at functionalComponent

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
        repairParticipantsAndMapsToLocal andThen
        repairModuleMapsToRemote andThen
        repairAtModule andThen
        repairFunctionalComponentMapsToRemote
      ) at
      ofType(ModuleDefinition)


  lazy val instanceRewrite = repairAtModuleDefinition at
    cstrApp
}
