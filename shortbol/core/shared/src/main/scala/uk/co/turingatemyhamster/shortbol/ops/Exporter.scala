package uk.co.turingatemyhamster
package shortbol
package ops

import datatree._
import datatree.{ast => da}
import relations._
import web._
import shortbol.{shorthandAst => sa}
import uk.co.turingatemyhamster.shortbol.longhandAst.SBFile
import uk.co.turingatemyhamster.shortbol.shorthandAst.IntegerLiteral
import terms.RDF.{about => rdf_about}

/**
 * Created by nmrp3 on 23/09/15.
 */
case class Exporter[T, E](export: T => E)

object Exporter {
  def apply[DT <: Datatree](c: EvalContext)(implicit _dtDSL: DatatreeDSL[DT], _webDSL: WebDSL[DT], _relDSL: RelationsDSL[DT]) = new ExporterEnv[DT] {
    val dtDSL = _dtDSL
    val webDSL = _webDSL
    val relDSL = _relDSL

    override def ctxt = c
  }

  implicit class ExporterOps[T](val _t: T) extends AnyVal {
    def export[E](implicit ev: Exporter[T, E]): E = ev.export(_t)
  }
}

trait ExporterEnv[DT <: Datatree] {

  implicit val dtDSL: DatatreeDSL[DT]
  implicit val webDSL: WebDSL[DT]
  implicit val relDSL: RelationsDSL[DT]

  import dtDSL._
  import webDSL._
  import relDSL._

  import webDSL.Methods._
  import dtDSL.Members._

  import Exporter._

  def apply(tl: SBFile) = topLevel_instances.export(tl.tops)

  def ctxt: EvalContext

  lazy val bindings = {
    ctxt.prgms.get(pragma.PrefixPragma.ID).to[List].flatten.map {
      case shorthandAst.Pragma(_, Seq(shorthandAst.ValueExp.Identifier(shorthandAst.LocalName(pfx)), shorthandAst.ValueExp.Identifier(shorthandAst.Url(url)))) =>
        pfx -> Uri(url)
    }
  }.toMap

  lazy val nsBindings = (bindings map { case (pfx, uri) => NamespaceBinding(Namespace(uri), Prefix(pfx)) }).to[Seq]

  def qnameToUri(qName: shorthandAst.QName) = bindings(qName.prefix.pfx) extendWith qName.localName.name

  implicit def listExporter[T, E](implicit ev: Exporter[T, E]): Exporter[List[T], List[E]] =
    Exporter { _ map ev.export }

//  def identityFor(i: shorthandAst.ConstructorApp) = i.body.collectFirst {
//    case shorthandAst.BodyStmt.PropertyExp(shorthandAst.PropertyExp(`rdf_about`, shorthandAst.PropertyValue.Reference(v))) => v.export[DT#Uri]
//  }

  def identityFor(i: longhandAst.ConstructorApp) = i.body.collectFirst {
    case longhandAst.PropertyExp(`rdf_about`, longhandAst.PropertyValue.Reference(v)) => v.export[DT#Uri]
  }

  implicit val topLevel_instances: Exporter[Seq[longhandAst.InstanceExp], DT#DocumentRoot] =
    Exporter { (ts: Seq[longhandAst.InstanceExp]) =>
      DocumentRoot(ZeroMany(nsBindings :_*), ZeroMany(ts map (_.export) :_*))
    }

  implicit val topLevelInstanceExpExporter: Exporter[longhandAst.InstanceExp, DT#TopLevelDocument] =
    Exporter { (t: longhandAst.InstanceExp) =>
      TopLevelDocument(
        ZeroMany(),
        ZeroOne.fromOption(identityFor(t.cstrApp)),
        One(t.cstrApp.cstr.export),
        ZeroMany(t.cstrApp.body.export.flatten :_*))
  }

  implicit val constructorAppExporter: Exporter[longhandAst.ConstructorApp, DT#NestedDocument] =
    Exporter { (t: longhandAst.ConstructorApp) =>
      NestedDocument(
        ZeroMany(),
        ZeroOne.fromOption(identityFor(t)),
        One(t.cstr.export),
        ZeroMany(t.body.export.flatten :_*))
    }

  implicit val identifierToUri: Exporter[shorthandAst.Identifier, DT#Uri] =
    Exporter { (i: shorthandAst.Identifier) =>
      i match {
        case shorthandAst.Url(url) =>
          Uri(url)
        case qn : shorthandAst.QName =>
          qnameToUri(qn)
        case ln : shorthandAst.LocalName =>
          throw new IllegalStateException(
            s"Unable to export a local name as an instance URI: ${ln.name}${if(ln.region != null) s" at ${ln.region.pretty}" else ""}")
      }
    }

  implicit val identifierToQName: Exporter[shorthandAst.Identifier, DT#QName] =
    Exporter { (i: shorthandAst.Identifier) =>
      i match {
        case qn@shorthandAst.QName(shorthandAst.NSPrefix(pfx), shorthandAst.LocalName(ln)) =>
          QName(qnameToUri(qn), LocalName(ln), Prefix(pfx))
        case _ =>
          throw new IllegalStateException(
            s"Unable to export identifier as a type qname: $i at ${i.region.pretty}")
      }
    }

  // fixme: remove this when possible
//  implicit val tpeConstructorExporter_shorthand: Exporter[shorthandAst.TpeConstructor, DT#QName] =
//    Exporter { (t: shorthandAst.TpeConstructor) =>
//      t match {
//        case shorthandAst.TpeConstructor1(id, _) =>
//          id.export[DT#QName]
//        case shorthandAst.TpeConstructorStar() =>
//          throw new IllegalStateException(
//            s"Unable to export a star constructor: $t at ${t.region.pretty}")
//      }
//    }

  implicit val tpeConstructorExporter: Exporter[longhandAst.TpeConstructor, DT#QName] =
    Exporter { (t: longhandAst.TpeConstructor) =>
      t.tpe.export[DT#QName]
    }

//  implicit val bodyStmtExporter: Exporter[shorthandAst.BodyStmt, Option[DT#NamedProperty]] =
//    Exporter { (b: shorthandAst.BodyStmt) =>
//      b match {
//        case shorthandAst.BodyStmt.PropertyExp(shorthandAst.PropertyExp(`rdf_about`, _)) =>
//          None
//        case shorthandAst.BodyStmt.PropertyExp(pe) =>
//          Some(pe.export)
//        case _ =>
//          None
//      }
//    }

//  implicit val propertyExpExporter: Exporter[shorthandAst.PropertyExp, DT#NamedProperty] =
//    Exporter { (pe: shorthandAst.PropertyExp) =>
//      val prop = pe.property.export[DT#QName]
//      pe.value match {
//        case shorthandAst.PropertyValue.Literal(l) =>
//          NamedProperty(ZeroMany(), prop, l.export)
//        case shorthandAst.PropertyValue.Reference(r) =>
//          NamedProperty(ZeroMany(), prop, UriLiteral(r.export[DT#Uri]))
//        case shorthandAst.PropertyValue.Nested(n) =>
//          NamedProperty(ZeroMany(), prop, n.export)
//      }
//    }

  implicit val propertyExpExporter: Exporter[longhandAst.PropertyExp, Option[DT#NamedProperty]] =
    Exporter { (pe: longhandAst.PropertyExp) =>
      pe.property match {
        case `rdf_about` =>
          None
        case p =>
          val prop = p.export[DT#QName]
          pe.value match {
            case longhandAst.PropertyValue.Literal(l) =>
              Some(NamedProperty(ZeroMany(), prop, l.export))
            case longhandAst.PropertyValue.Reference(r) =>
              Some(NamedProperty(ZeroMany(), prop, UriLiteral(r.export[DT#Uri])))
            case longhandAst.PropertyValue.Nested(n) =>
              Some(NamedProperty(ZeroMany(), prop, n.export))
          }
      }
    }

  implicit val literalExporter: Exporter[shorthandAst.Literal, DT#Literal] =
    Exporter { (l: shorthandAst.Literal) =>
      l match {
        case shorthandAst.IntegerLiteral(i) =>
          LongLiteral(i)
        case shorthandAst.StringLiteral(s, dt, lang) =>
          dtDSL.TypedLiteral(One(s.export), ZeroOne.fromOption(dt map (_.tpe.export[DT#Uri])), ZeroOne.fromOption(lang.map(_.tag)))
      }
    }

  implicit val styleExporter: Exporter[shorthandAst.StringLiteral.Style, String] =
    Exporter { (s: shorthandAst.StringLiteral.Style) =>
      s match {
        case shorthandAst.StringLiteral.SingleLine(txt, _) =>
          txt
        case shorthandAst.StringLiteral.MultiLine(txts, _) =>
          txts.mkString
      }
    }
}