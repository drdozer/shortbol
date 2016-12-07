package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import scalaz._
import Scalaz._
import monocle._
import Monocle._
import shorthandAst.sugar._
import longhandAst.sugar._
import RewriteRule.{*, Rewritten, allElements, ofType}
import optics.{longhand => ol}
import ol.SBFile._
import ol.InstanceExp._
import ol.ConstructorApp._
import ol.TpeConstructor._
import ol.PropertyValue._
import ol.PropertyValue.Nested.{value => nestedValue}
import ol.PropertyValue.Reference.{value => referenceValue}
import longhandAst.{InstanceExp, PropertyExp, PropertyValue}
import pragma.DefaultPrefixPragma
import shorthandAst.{Datatype, Identifier, Literal, LocalName, QName, StringLiteral}
import terms.RDF
import terms.SBOL._
import terms.EDAM
import PropertyStep.PropertyStepOps
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState

object RepairSequence extends InstanceRewriter {

  lazy val fastaToDNA = RewriteRule ({
    case StringLiteral(style, Some(Datatype(EDAM.fasta)), _) =>
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
    case StringLiteral(style, Some(Datatype(EDAM.genbank)), _) =>
      val s = style.asString

      StringLiteral(
        StringLiteral.SingleLine(s.replaceAll("[\\s\\d]+", "")),
        None,
        None)
  } : PartialFunction [Literal, Literal])

  lazy val repairToDNA = fastaToDNA or genbankToDNA

  lazy val repairAtSequence = repairToDNA at
    elements at
    ofType(Sequence)

  override def instanceRewrite: RewriteRule[InstanceExp] =
    repairAtSequence at cstrApp
}

object RepairComponentDefinition extends InstanceRewriter {

  lazy val hoistNestedSequence = RewriteRule { (pv: PropertyValue) =>
    (asNested composeLens nestedValue) getOrModify pv fold (
      _ => pv.left[Rewritten[PropertyValue]].point[EvalState],
      nested => for {
        nextId <- Eval.nextIdentifier
        seqId <- DefaultPrefixPragma.rewrite(nextId)
        ref = PropertyValue.Reference(seqId) : PropertyValue
        newSeq = InstanceExp(seqId, nested)
      } yield ref.set(newSeq::Nil).right[PropertyValue]
    )
  } at sequence

  lazy val repairConstraints = RewriteRule { (ps: List[PropertyExp]) =>
    val defToAbout = (for {
      cds <- ps collect { case PropertyExp(`component`, PropertyValue.Nested(ca)) => ca }
      defnt <- cds.body collect { case PropertyExp(`definition`, PropertyValue.Reference(r)) => r }
      about <- cds.body collect { case PropertyExp(RDF.about, PropertyValue.Reference(r)) => r }
    } yield (defnt, about)).toMap

    RewriteRule { (ref: PropertyValue.Reference) =>
      defToAbout get ref.value map PropertyValue.Reference
    } at ((sequenceConstraint, sequenceAnnotation) --> *)
  }

  lazy val componentsForRefs = RepairOps build { (ref: Identifier) =>
    component := Component(
      access := access_public,
      definition := ref
    )
  } from
    (sequenceConstraint --> (subject, `object`),
      sequenceAnnotation --> component) excluding
    (component --> (definition, RDF.about))

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
        Component(
          displayId := lnO map slLit,
          access := access_public,
          definition := ref)) : PropertyValue
    }
  }

  lazy val repairAtComponent = replaceComponentReferenceWithComponent at
    component

  lazy val repairAtComponentDefinition = (
    hoistNestedSequence andThen
      repairAtComponent andThen
      repairConstraints andThen
      componentsForRefs) at
    ofType(ComponentDefinition)

  lazy val instanceRewrite =
    repairAtComponentDefinition at cstrApp
}