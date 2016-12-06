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

/**
  * Created by nmrp3 on 17/11/16.
  */
object RepairModule extends InstanceRewriter {

  private def defToAbout(ps: List[PropertyExp]): Map[Identifier, Identifier] = (for {
    cds <- ps collect { case PropertyExp(`functionalComponent`, PropertyValue.Nested(ca)) => ca }
    defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
    about <- cds.body collect { case PropertyExp(RDF.about, PropertyValue.Reference(r)) => r }
  } yield (defnt, about)).toMap

  lazy val repairParticipantsAndMapsToLocal = RewriteRule { (ps: List[PropertyExp]) =>
    val d2a = defToAbout(ps)

    RewriteRule { (ref: PropertyValue.Reference) =>
      d2a get ref.value map PropertyValue.Reference
    } at (
      interaction --> participation --> participant,
      module --> mapsTo --> local
    )
  }

  // @ module: Module
  //
  // Follow `definition` to an instance of ModuleDefinition.
  // @ mapsTo
  // find
  lazy val repairMapsToRemote = RewriteRule { (ps: List[PropertyExp]) =>
    for {
      remoteRef <- (RepairOps deReference definition in ps).point[Eval.EvalState]
      _ = println(s"remoteRef: $remoteRef")
      remoteMDs <- remoteRef.traverseU(r => Eval.inst(r).map(_.to[List]))
      _ = println(s"remote module definition: $remoteMDs")
    } yield {
      // There should be 0 or 1 remoteModuleDefinitions that is the resolved ModuleDefinition for definition
      // Chase all the functionalComponents
      // Extract definition -> rdf:about


      val d2a = remoteMDs.flatten.map(i => defToAbout(i.cstrApp.body)).foldLeft(Map.empty[Identifier, Identifier])(_ ++ _)
      println(s"build d2a dictionary: $d2a")

      RewriteRule { (ref: PropertyValue.Reference) =>
        println(s"Resolving ${ref.value} to ${d2a.get(ref.value)}")
        d2a get ref.value map PropertyValue.Reference
      } at (
        mapsTo --> remote
      )
    }
  } at module


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
        repairMapsToRemote andThen
        repairAtModule
      ) at
      ofType(ModuleDefinition)


  lazy val instanceRewrite = repairAtModuleDefinition at
    cstrApp
}