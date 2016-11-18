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
import uk.co.turingatemyhamster.shortbol.longhandAst.{ConstructorApp, InstanceExp, PropertyExp, PropertyValue}
import uk.co.turingatemyhamster.shortbol.pragma.DefaultPrefixPragma
import uk.co.turingatemyhamster.shortbol.shorthandAst.{Datatype, Literal, StringLiteral, Identifier, QName, LocalName}

/**
  * Created by nmrp3 on 17/11/16.
  */
object RepairModule {

  private final val interaction = "sbol" :# "interaction"
  private final val participation = "sbol" :# "participation"
  private final val participant = "sbol" :# "participant"
  private final val functionalComponent = "sbol" :# "functionalComponent"
  private final val FunctionalComponent = "sbol" :# "FunctionalComponent"
  final private val access = "sbol" :# "access"
  final private val access_public = "sbol" :# "public"
  private final val definition = "sbol" :# "definition"
  private final val direction = "sbol" :# "direction"
  private final val inout = "sbol" :# "inout"
  private final val rdf_about = "rdf" :# "about"
  private final val ModuleDefinition = "sbol" :# "ModuleDefinition"

  lazy val repairParticipants = RewriteRule { (ps: List[PropertyExp]) =>
    val defToAbout = (for {
      cds <- ps collect { case PropertyExp(`functionalComponent`, PropertyValue.Nested(ca)) => ca }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
      about <- cds.body collect { case PropertyExp(`rdf_about`, PropertyValue.Reference(r)) => r }
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
      abs <- cds.body collect { case PropertyExp(`rdf_about`, PropertyValue.Reference(r)) => r }
    } yield abs

    val orphaned = fromParticipants -- defs -- abouts

    val cmpts = for {
      ref <- orphaned.to[List]
      cmpt <- functionalComponent := ConstructorApp(
        FunctionalComponent,
        access := access_public,
        definition := ref,
        direction := inout
      )
    } yield cmpt

    println(s"Participants: $fromParticipants")
    println(s"defs: $defs")
    println(s"abouts: $abouts")
    println(s"orphaned: $orphaned")
    println(s"cmpts: $cmpts")

    if(cmpts.isEmpty) None else Some(cmpts ::: ps)
  } log "componentsForRefs"

  lazy val repairAtModuleDefinition = (componentsForRefs andThen repairParticipants) at
    body log "body" at
    ((cstr composeLens tpe) :== ModuleDefinition) log "repairAtModuleDefinition"

  lazy val repairAll = repairAtModuleDefinition at
    cstrApp at
    allElements at
    tops
}
