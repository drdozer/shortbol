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

object RepairComponents {

  lazy val repairAll = (RepairSequence.repairAtSequence or RepairComponentDefinition.repairAtComponentDefinition) at
    cstrApp at
    allElements at
    tops
}


object RepairSequence {

  final private val EdamFasta = "edam" :# "fasta"
  final private val EdamGenbank = "edam" :# "genbank"
  final private val XsdString = "xsd" :# "string"
  final private val elements = "sbol" :# "elements"
  final private val Sequence = "sbol" :# "Sequence"

  lazy val fastaToDNA = RewriteRule ({
    case StringLiteral(style, Some(Datatype(EdamFasta)), _) =>
      val s = style.asString
      val trimmed = if (s startsWith ">") {
        s.substring(s.indexOf('\n') + 1)
      } else s

      StringLiteral(
        StringLiteral.SingleLine(trimmed.replaceAll("[\\s]+", "")),
        None,
        None)
  }: PartialFunction[Literal, Literal])

  lazy val genbankToDNA = RewriteRule ({
    case StringLiteral(style, Some(Datatype(EdamGenbank)), _) =>
      val s = style.asString

      StringLiteral(
        StringLiteral.SingleLine(s.replaceAll("[\\s\\d]+", "")),
        None,
        None)
  } : PartialFunction [Literal, Literal])

  lazy val repairToDNA = fastaToDNA or genbankToDNA

  lazy val repairAtElement = repairToDNA at
    ol.PropertyValue.Literal.value at
    asLiteral at
    value at
    (property :== elements)

  lazy val repairAtConstructorApp = repairAtElement at
    allElements at
    body

  lazy val repairAtSequence = repairAtConstructorApp at
    ((cstr composeLens tpe) :== Sequence)
}

object RepairComponentDefinition {
  final private val displayId = "sbol" :# "displayId"
  final private val sequence = "sbol" :# "sequence"
  final private val component = "sbol" :# "component"
  final private val access = "sbol" :# "access"
  final private val access_public = "sbol" :# "public"
  final private val definition = "sbol" :# "definition"
  final private val rdf_about = "rdf" :# "about"
  final private val sequenceConstraint = "sbol" :# "sequenceConstraint"
  final private val subject = "sbol" :# "subject"
  final private val `object` = "sbol" :# "object"
  final private val sequenceAnnotation = "sbol" :# "sequenceAnnotation"

  final private val ComponentDefinition = "sbol" :# "ComponentDefinition"
  final private val Component = "sbol" :# "Component"
  final private val SequenceConstraint = "sbol" :# "SequenceConstraint"

  lazy val hoistNestedSequence = RewriteRule { (pv: PropertyValue) =>
    for {
      nested <- (asNested composeLens nestedValue) getOrModify pv
    } yield {
      val refForNested = for {
        nextId <- Eval.nextIdentifier
        seqId <- DefaultPrefixPragma.rewrite(nextId)
      } yield seqId
      for {
        seqId <- refForNested.lift : RewriteRule.Rewritten[Identifier]
        ref = PropertyValue.Reference(seqId) : PropertyValue
        newSeq = InstanceExp(seqId, nested)
        refWithSeq <- Rewritten(ref.set(newSeq::Nil))
      } yield refWithSeq
    }
  }

  lazy val repairConstraints = RewriteRule { (ps: List[PropertyExp]) =>
    val defToAbout = (for {
      cds <- ps collect { case PropertyExp(`component`, PropertyValue.Nested(ca)) => ca }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
      about <- cds.body collect { case PropertyExp(`rdf_about`, PropertyValue.Reference(r)) => r }
    } yield (defnt, about)).toMap

    val repairRef = RewriteRule { (ref: PropertyValue.Reference) =>
      defToAbout get ref.value map PropertyValue.Reference
    } at asReference at
      value at
      allElements at
      body at
      nestedValue at
      asNested at
      value

    val repairSeqCons = repairRef at
      (property :== sequenceConstraint)

    val repairSeqAnn = repairRef at
      (property :== sequenceAnnotation)

    (repairSeqCons andThen repairSeqAnn) at allElements
  }

  lazy val componentsForRefs = RewriteRule { (ps: List[PropertyExp]) =>
    val fromScs = (for {
      scs <- ps collect { case PropertyExp(`sequenceConstraint`, PropertyValue.Nested(sc)) => sc }
      ref <- scs.body.collect {
        case PropertyExp(`subject`, PropertyValue.Reference(r)) => r
        case PropertyExp(`object`, PropertyValue.Reference(r)) => r
      }
    } yield ref).to[Set]

    val fromSans = (for {
      scs <- ps collect { case PropertyExp(`sequenceAnnotation`, PropertyValue.Nested(sc)) => sc }
      ref <- scs.body.collect {
        case PropertyExp(`component`, PropertyValue.Reference(r)) => r
      }
    } yield ref).to[Set]

    val defs = for {
      cds <- ps collect { case PropertyExp(`component`, PropertyValue.Nested(ca)) => ca }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
    } yield defnt

    val abouts = for {
      cds <- ps collect { case PropertyExp(`component`, PropertyValue.Nested(ca)) => ca }
      abs <- cds.body collect { case PropertyExp(`rdf_about`, PropertyValue.Reference(r)) => r }
    } yield abs

    val orphaned = fromScs ++ fromSans -- defs -- abouts

    val cmpts = for {
      ref <- orphaned.to[List]
      cmpt <- component := ConstructorApp(
        Component,
        access := access_public,
        definition := ref
      )
    } yield cmpt

    if(cmpts.isEmpty) None else Some(cmpts ::: ps)
  }


  lazy val repairAtSequence = hoistNestedSequence at
    value at
    (property :== sequence)

  lazy val replaceComponentReferenceWithComponent = RewriteRule { (pv: PropertyValue) =>
    for {
      ref <- (asReference composeLens referenceValue) getOrModify pv
    } yield {
      val lnO = ref match {
        case LocalName(ln) => Some(ln)
        case QName(_, LocalName(ln)) => Some(ln)
        case _ => None
      }
      // fixme: pull out local name from ref, use as displayId if possible
      PropertyValue.Nested(
        ConstructorApp(
          Component,
          displayId := lnO map slLit,
          access := access_public,
          definition := ref)) : PropertyValue
    }
  }

  lazy val repairAtComponent = replaceComponentReferenceWithComponent at
    value at
    (property :== component)

  lazy val repairSequenceAndComponent = (repairAtSequence or repairAtComponent) at
      allElements

  lazy val repairAtConstructorApp = (repairSequenceAndComponent andThen repairConstraints andThen componentsForRefs) at
    body

  lazy val repairAtComponentDefinition = repairAtConstructorApp at
    ((cstr composeLens tpe) :== ComponentDefinition)
}