package uk.co.turingatemyhamster
package shortbol
package ops

import datatree._
import datatree.{ast => da}
import relations._
import web._
import shortbol.{ast => sa}
import uk.co.turingatemyhamster.shortbol.ast.{IntegerLiteral, SBEvaluatedFile}

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

  def apply(tl: SBEvaluatedFile) = topLevel_instances.export(tl.tops)

  def ctxt: EvalContext

  lazy val bindings = {
    ctxt.prgms.get(pragma.PrefixPragma.ID).to[List].flatten.map {
      case sa.Pragma(_, Seq(sa.ValueExp.Identifier(sa.LocalName(pfx)), sa.ValueExp.Identifier(sa.Url(url)))) =>
        pfx -> Uri(url)
    }
  }.toMap

  lazy val nsBindings = (bindings map { case (pfx, uri) => NamespaceBinding(Namespace(uri), Prefix(pfx)) }).to[Seq]

  def qnameToUri(qName: sa.QName) = bindings(qName.prefix.pfx) extendWith qName.localName.name

  implicit def seqExporter[T, E](implicit ev: Exporter[T, E]): Exporter[Seq[T], Seq[E]] =
    Exporter { _ map ev.export }

  def identityFor(i: sa.InstanceExp) = i.cstrApp.body.collectFirst {
    case sa.BodyStmt.Assignment(sa.Assignment(`rdf_about`, sa.ValueExp.Identifier(v))) => v.export[DT#Uri]
  }

  implicit val topLevel_instances: Exporter[Seq[sa.TopLevel.InstanceExp], DT#DocumentRoot] =
    Exporter { (ts: Seq[sa.TopLevel.InstanceExp]) =>
      DocumentRoot(ZeroMany(nsBindings :_*), ZeroMany(ts map (_.export) :_*))
    }

  implicit val topLevelInstanceExpExporter: Exporter[sa.TopLevel.InstanceExp, DT#TopLevelDocument] =
    Exporter { (t: sa.TopLevel.InstanceExp) =>
      TopLevelDocument(
        ZeroMany(),
        ZeroOne.fromOption(identityFor(t.instanceExp) orElse Some(t.instanceExp.id.export[DT#Uri])),
        One(t.instanceExp.cstrApp.cstr.export),
        ZeroMany(t.instanceExp.cstrApp.body.export.flatten :_*))
  }

  implicit val instanceUriExpExporter: Exporter[sa.InstanceExp, (DT#QName, DT#NestedDocument)] =
    Exporter { (t: sa.InstanceExp) =>
      (t.id.export[DT#QName],
        NestedDocument(
          ZeroMany(),
          ZeroOne.fromOption(identityFor(t)),
          One(t.cstrApp.cstr.export),
          ZeroMany(t.cstrApp.body.export.flatten :_*))
        )
    }

  implicit val identifierToUri: Exporter[sa.Identifier, DT#Uri] =
    Exporter { (i: sa.Identifier) =>
      i match {
        case sa.Url(url) =>
          Uri(url)
        case qn : sa.QName =>
          qnameToUri(qn)
        case ln : sa.LocalName =>
          throw new IllegalStateException(
            s"Unable to export a local name as an instance URI: ${ln.name} at ${ln.region.pretty}")
      }
    }

  implicit val identifierToQName: Exporter[sa.Identifier, DT#QName] =
    Exporter { (i: sa.Identifier) =>
      i match {
        case qn@sa.QName(sa.NSPrefix(pfx), sa.LocalName(ln)) =>
          QName(qnameToUri(qn), LocalName(ln), Prefix(pfx))
        case _ =>
          throw new IllegalStateException(
            s"Unable to export identifier as a type qname: $i at ${i.region.pretty}")
      }
    }

  implicit val tpeConstructorExporter: Exporter[sa.TpeConstructor, DT#QName] =
    Exporter { (t: sa.TpeConstructor) =>
      t match {
        case sa.TpeConstructor1(id, _) =>
          id.export[DT#QName]
        case sa.TpeConstructorStar() =>
          throw new IllegalStateException(
            s"Unable to export a star constructor: $t at ${t.region.pretty}")
      }
    }

  implicit val bodyStmtExporter: Exporter[sa.BodyStmt, Option[DT#NamedProperty]] =
    Exporter { (b: sa.BodyStmt) =>
      b match {
        case sa.BodyStmt.Assignment(sa.Assignment(`rdf_about`, _)) =>
          None
        case sa.BodyStmt.Assignment(a) =>
          Some(a.export)
        case sa.BodyStmt.InstanceExp(i) =>
          val (name, value) = i.export
          Some(NamedProperty(ZeroMany(), name, value))
        case sa.BodyStmt.ConstructorApp(c) =>
          throw new IllegalStateException(
            s"Unable to export constructor application within body statment: $c at ${c.region.pretty}")
        case _ =>
          None
      }
    }

  implicit val assignmentExporter: Exporter[sa.Assignment, DT#NamedProperty] =
    Exporter { (a: sa.Assignment) =>
      NamedProperty(ZeroMany(), a.property.export[DT#QName], a.value.export)
    }

  implicit val valueExpExporter: Exporter[sa.ValueExp, DT#PropertyValue] =
    Exporter { (ve: sa.ValueExp) =>
      ve match {
        case sa.ValueExp.Identifier(i) =>
          UriLiteral(i.export[DT#Uri])
        case sa.ValueExp.Literal(l) =>
          l.export
      }
    }

  implicit val literalExporter: Exporter[sa.Literal, DT#Literal] =
    Exporter { (l: sa.Literal) =>
      l match {
        case sa.IntegerLiteral(i) =>
          LongLiteral(i)
        case sa.StringLiteral(s, dt, lang) =>
          dtDSL.TypedLiteral(One(s.export), ZeroOne.fromOption(dt map (_.tpe.export[DT#Uri])), ZeroOne.fromOption(lang.map(_.tag)))
      }
    }

  implicit val styleExporter: Exporter[sa.StringLiteral.Style, String] =
    Exporter { (s: sa.StringLiteral.Style) =>
      s match {
        case sa.StringLiteral.SingleLine(txt, _) =>
          txt
        case sa.StringLiteral.MultiLine(txts, _) =>
          txts.mkString
      }
    }

  import sa.sugar._
  val rdf_about = "rdf" :# "about"
}