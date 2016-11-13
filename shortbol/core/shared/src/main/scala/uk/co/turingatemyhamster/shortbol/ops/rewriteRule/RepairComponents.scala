package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import scalaz._
import Scalaz._
import shorthandAst.{Datatype, Literal, StringLiteral}
import shorthandAst.sugar._
import longhandAst.sugar._
import RewriteRule.Filtering
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
import uk.co.turingatemyhamster.shortbol.pragma.DefaultPrefixPragma

object RepairComponents {

  lazy val repairAll = (RepairSequence.repairAtSequence or RepairComponentDefinition.repairAtComponentDefinition) at
    cstrApp at
    allElements at
    tops
}


object RepairSequence {

  val EdamFasta = "edam" :# "fasta"
  val EdamGenbank = "edam" :# "genbank"
  val XsdString = "xsd" :# "string"
  val elements = "sbol" :# "elements"
  val Sequence = "sbol" :# "Sequence"

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
  val sequence = "sbol" :# "sequence"
  val component = "sbol" :# "component"
  val access = "sbol" :# "access"
  val access_public = "sbol" :# "public"
  val definition = "sbol" :# "definition"

  val ComponentDefinition = "sbol" :# "ComponentDefinition"
  val Component = "sbol" :# "Component"

  lazy val hoistNestedSequence = RewriteRule { (pv: longhandAst.PropertyValue) =>
    for {
      nested <- (asNested composeLens nestedValue) getOrModify pv
    } yield {
      val refForNested = for {
        nextId <- Eval.nextIdentifier
        seqId <- DefaultPrefixPragma.rewrite(nextId)
      } yield longhandAst.PropertyValue.Reference(seqId) : longhandAst.PropertyValue
      val x = for {
        ref <- refForNested.lift : RewriteRule.Rewritten[longhandAst.PropertyValue]
        _ <- Writer.set(nested).point[RewriteRule.Rewritten]
      } yield ref
      x : RewriteRule.Rewritten[longhandAst.PropertyValue]
    }
  }

  lazy val repairAtSequence = hoistNestedSequence at
    value at
    (property :== sequence)

  lazy val replaceComponentReferenceWithComponent = RewriteRule { (pv: longhandAst.PropertyValue) =>
    for {
      ref <- (asReference composeLens referenceValue) getOrModify pv
    } yield {
      longhandAst.PropertyValue.Nested(
        longhandAst.ConstructorApp(
          Component,
          access := access_public,
          definition := ref)) : longhandAst.PropertyValue
    }
  }

  lazy val repairAtComponent = replaceComponentReferenceWithComponent at
    value at
    (property :== component)

  lazy val repairAtConstructorApp = (repairAtSequence or repairAtComponent) at
    allElements at
    body

  lazy val repairAtComponentDefinition = repairAtConstructorApp at
    ((cstr composeLens tpe) :== ComponentDefinition)
}