package uk.co.turingatemyhamster
package shortbol
package ops

import datatree._
import relations._
import web._
import shortbol.{ast => sa}

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

  import Exporter._

  def apply(tl: Seq[sa.TopLevel.InstanceExp]) = topLevel_instances.export(tl)

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

  implicit val topLevel_instances: Exporter[Seq[sa.TopLevel.InstanceExp], DT#DocumentRoot] =
    Exporter { (ts: Seq[sa.TopLevel.InstanceExp]) =>
      DocumentRoot(ZeroMany(nsBindings :_*), ZeroMany(ts map (_.export) :_*))
    }

  implicit val topLevelInstanceExpExporter: Exporter[sa.TopLevel.InstanceExp, DT#TopLevelDocument] =
    Exporter { (t: sa.TopLevel.InstanceExp) =>
      TopLevelDocument(
        ZeroMany(),
        ZeroOne(t.instanceExp.id.export[DT#Uri]),
        One(t.instanceExp.cstrApp.cstr.export),
        ZeroMany(t.instanceExp.cstrApp.body.export :_*))
  }

  implicit val instanceUriExpExporter: Exporter[sa.InstanceExp, DT#NestedDocument] =
    Exporter { (t: sa.InstanceExp) =>
      NestedDocument(
        ZeroMany(),
        ZeroOne(t.id.export[DT#Uri]),
        One(t.cstrApp.cstr.export),
        ZeroMany(t.cstrApp.body.export :_*))
    }

  implicit val identifierToUri: Exporter[sa.Identifier, DT#Uri] =
    Exporter { (i: sa.Identifier) =>
      i match {
        case sa.Url(url) =>
          Uri(url)
        case qn : sa.QName =>
          qnameToUri(qn)
        case ln : sa.LocalName =>
          throw new IllegalStateException(s"Unable to export a local name as an instance URI: ${ln.name} from ${ln.region}")
      }
    }

  implicit val identifierToQName: Exporter[sa.Identifier, DT#QName] =
    Exporter { (i: sa.Identifier) =>
      i match {
        case sa.QName(sa.NSPrefix(pfx), sa.LocalName(ln)) =>
          QName(null, LocalName(ln), Prefix(pfx))
        case _ =>
          throw new IllegalStateException(s"Unable to export identifier as a type qname: $i from ${i.region}")
      }
    }

  implicit val tpeConstructorExporter: Exporter[sa.TpeConstructor, DT#QName] =
    Exporter { (t: sa.TpeConstructor) =>
      t match {
        case sa.TpeConstructor1(id, _) =>
          id.export[DT#QName]
        case sa.TpeConstructorStar() =>
          throw new IllegalStateException(s"Unable to export a star constructor: $t from ${t.region}")
      }
    }

  implicit val bodyStmtExporter: Exporter[sa.BodyStmt, DT#NamedProperty] = ???
}