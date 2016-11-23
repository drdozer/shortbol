package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import scalaz._
import Scalaz._
import monocle._
import Monocle._
import shorthandAst.sugar._
import longhandAst.sugar._
import RewriteRule.{Filtering, Rewritten}
import RewriteAt.allElements
import optics.{longhand => ol}
import ol.SBFile._
import ol.InstanceExp._
import ol.ConstructorApp._
import ol.TpeConstructor._
import ol.PropertyValue._
import ol.PropertyExp._
import ol.PropertyValue.Nested.{value => nestedValue}
import ol.PropertyValue.Reference.{value => referenceValue}
import longhandAst.{ConstructorApp, InstanceExp, PropertyExp, PropertyValue}
import pragma.DefaultPrefixPragma
import shorthandAst.{Datatype, Identifier, Literal, LocalName, QName, StringLiteral}
import terms.RDF
import terms.SBOL._

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
    } at asReference at
      value at
      (property :== participant) at
      allElements at
      body at
      nestedValue at
      asNested at
      value at
      (property :== participation) at
      allElements at
      body at
      nestedValue at
      asNested at
      value at
      (property :== interaction) at
      allElements
  }


  lazy val componentsForRefs = RewriteRule { (ps: List[PropertyExp]) =>
    val fromParticipants = (for {
      interactions <- ps collect { case PropertyExp(`interaction`, PropertyValue.Nested(i)) => i }
      participations <- interactions.body collect { case PropertyExp(`participation`, PropertyValue.Nested(i)) => i }
      ref <- participations.body collect { case PropertyExp(`participant`, PropertyValue.Reference(r)) => r }
    } yield ref).to[Set]

    val defs = for {
      cds <- ps collect { case PropertyExp(`functionalComponent`, PropertyValue.Nested(c)) => c }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
    } yield defnt

    val abouts = for {
      cds <- ps collect { case PropertyExp(`functionalComponent`, PropertyValue.Nested(ca)) => ca }
      abs <- cds.body collect { case PropertyExp(RDF.about, PropertyValue.Reference(r)) => r }
    } yield abs

    val orphaned = fromParticipants -- defs -- abouts

    val cmpts = for {
      ref <- orphaned.to[List]
      cmpt <- functionalComponent := FunctionalComponent(
        access := access_public,
        definition := ref,
        direction := inout
      )
    } yield cmpt

    if(cmpts.isEmpty) None else Some(cmpts ::: ps)
  }

  lazy val replaceComponentReferenceWithComponent = RewriteRule { (pv: PropertyValue) =>
    for {
      ref <- (asReference composeLens referenceValue) getOrModify pv
    } yield {
      val lnO = ref match {
        case LocalName(ln) => Some(ln)
        case QName(_, LocalName(ln)) => Some(ln)
        case _ => None
      }

      PropertyValue.Nested(
        FunctionalComponent(
          displayId := lnO map slLit,
          access := access_public,
          definition := ref,
          direction := inout)) : PropertyValue
    }
  }

  lazy val repairAtComponent = replaceComponentReferenceWithComponent at
    value at
    (property :== functionalComponent) at
    allElements

  lazy val replaceModuleReferenceWithModule = RewriteRule { (pv: PropertyValue) =>
    for {
      ref <- (asReference composeLens referenceValue) getOrModify pv
    } yield {
      val lnO = ref match {
        case LocalName(ln) => Some(ln)
        case QName(_, LocalName(ln)) => Some(ln)
        case _ => None
      }

      PropertyValue.Nested(
        Module(
          displayId := lnO map slLit,
          definition := ref
        )
      ) : PropertyValue
    }
  }

  lazy val repairAtModule = replaceModuleReferenceWithModule at
    value at
    (property :== module) at
    allElements

  lazy val repairAtModuleDefinition =
    (
      repairAtComponent andThen
        componentsForRefs andThen
        repairParticipants andThen
        repairAtModule
      ) at
      body at
      ((cstr composeLens tpe) :== ModuleDefinition)

  lazy val repairAll = repairAtModuleDefinition at
    cstrApp at
    allElements at
    tops
}
