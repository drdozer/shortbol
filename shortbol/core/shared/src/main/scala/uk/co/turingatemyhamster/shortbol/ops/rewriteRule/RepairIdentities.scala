package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import shorthandAst.sugar._
import longhandAst.{ConstructorApp, InstanceExp, PropertyExp, SBFile}
import longhandAst.sugar._
import RewriteAt.allElements
import pragma.DefaultPrefixPragma

/**
  *
  *
  * @author Matthew Pocock
  */
object RepairIdentities {
  final private val displayId = "sbol" :# "displayId"
  final private val rdf_about = "rdf" :# "about"

  final private val noDisplayId = (_: List[PropertyExp]).forall(_.property != displayId)
  final private val noAbout = (_: List[PropertyExp]).forall(_.property != rdf_about)

  import optics.longhand.InstanceExp._
  import optics.longhand.ConstructorApp._
  import optics.longhand.SBFile._
  import optics.longhand.PropertyValue._
  import optics.longhand.PropertyExp._
  import Nested.{value => nestedValue}


  lazy val bodyRequiresDisplayId: RewriteRule[List[PropertyExp]] = RewriteRule { (ps: List[PropertyExp]) =>
    for {
      id <- Eval.nextIdentifier
    } yield (displayId := slLit(id.name)) ::: ps
  } at noDisplayId

  def bodyRequiresAbout(parentId: shorthandAst.Identifier): RewriteRule[List[PropertyExp]] = RewriteRule { (ps: List[PropertyExp]) =>
    for {
      longhandAst.PropertyExp(_, longhandAst.PropertyValue.Literal(shorthandAst.StringLiteral(s, _, _))) <- ps find (_.property == displayId)
      about <- parentId match {
        case shorthandAst.LocalName(_) =>
          None
        case shorthandAst.QName(pfx, shorthandAst.LocalName(ln)) =>
          Some(pfx :# s"$ln/${s.asString}")
        case shorthandAst.Url(url) =>
          Some(shorthandAst.Url(s"$url/${s.asString}"))
      }
    } yield {
      (rdf_about := about) ::: ps
    }
  }

  lazy val recurseOverBody: RewriteRule[List[PropertyExp]] = RewriteRule { (ps: List[PropertyExp]) =>
    for {
      longhandAst.PropertyExp(_, longhandAst.PropertyValue.Reference(about)) <- ps find (_.property == rdf_about)
    } yield
      (bodyRequiresDisplayId andThen bodyRequiresAbout(about) andThen recurseOverBody) at
        body at
        nestedValue at
        asNested at
        value at
        allElements
  }

  lazy val recursefromInstanceExp = recurseOverBody at body at cstrApp

  lazy val cstrAppRequiresDisplayId = bodyRequiresDisplayId at body

  lazy val instanceExpRequiersDisplayIdAndAbout: RewriteRule[InstanceExp] = RewriteRule { (ie: InstanceExp) =>
    RewriteRule { (bdy: List[PropertyExp]) =>
      for {
        id <- DefaultPrefixPragma.rewrite(ie.identifier)
      } yield {
        val withAbout = (rdf_about := id) ::: bdy
        id match {
          case shorthandAst.LocalName(ln) =>
            (displayId := slLit(ln)) ::: withAbout
          case shorthandAst.QName(_, shorthandAst.LocalName(ln)) =>
            (displayId := slLit(ln)) ::: withAbout
          case _ =>
            withAbout
        }
      }
    } at noDisplayId at noAbout at body at cstrApp
  }

  lazy val instanceExpRequiresAbout: RewriteRule[InstanceExp] = RewriteRule { (ie: InstanceExp) =>
    (cstrApp composeLens body) modify
      ((rdf_about := ie.identifier) ::: _) apply
      ie
  } at { (ie: InstanceExp) =>
    ie.cstrApp.body.collectFirst{ case PropertyExp(`rdf_about`, _) => () }.isEmpty
  }

  lazy val repairAll: RewriteRule[SBFile] = (
    instanceExpRequiersDisplayIdAndAbout andThen instanceExpRequiresAbout andThen recursefromInstanceExp) at
    allElements at tops
}
