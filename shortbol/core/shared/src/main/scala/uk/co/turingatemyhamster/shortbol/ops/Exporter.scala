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

  implicit class ExporterOps[T](val _t: T) extends AnyVal {
    def export[E](implicit ev: Exporter[T, E]): E = ev.export(_t)
  }

}

object ExporterEnv {
  def apply[DT <: Datatree](implicit _dtDSL: DatatreeDSL[DT], _webDSL: WebDSL[DT], _relDSL: RelationsDSL[DT]) = new ExporterEnv[DT] {
    val dtDSL = _dtDSL
    val webDSL = _webDSL
    val relDSL = _relDSL
  }
}

trait ExporterEnv[DT <: Datatree] {

  implicit val dtDSL: DatatreeDSL[DT]
  implicit val webDSL: WebDSL[DT]
  implicit val relDSL: RelationsDSL[DT]

  import dtDSL._
  import webDSL._
  import relDSL._

  import Exporter._

  def ctxt: EvalContext

  def nsBindings = ctxt.prgms.get(pragma.PrefixPragma.ID).to[List].flatten.map {
    case sa.Pragma(_, Seq(sa.ValueExp.Identifier(sa.LocalName(pfx)), sa.ValueExp.Identifier(sa.Url(url)))) =>
      NamespaceBinding(Namespace(Uri(url)), Prefix(pfx))
  }
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
        ZeroOne(t.instanceExp.id.export),
        One(t.instanceExp.cstrApp.cstr.export),
        ZeroMany())
  }

  implicit val instanceExpExporter: Exporter[sa.InstanceExp, DT#NestedDocument] =
    Exporter { (t: sa.InstanceExp) =>
      NestedDocument(
        ZeroMany(),
        ZeroOne(t.id.export),
        One(t.cstrApp.cstr.export),
        ZeroMany())
    }

  implicit val tpeConstructor: Exporter[sa.TpeConstructor, DT#QName] =
    Exporter[sa.TpeConstructor, DT#QName] { _.id.export }

  implicit val urlExporter: Exporter[sa.Url, DT#Uri] =
    Exporter { (u: sa.Url) =>
      u match {
        case sa.Url(ln) =>
          Uri(ln)
      }
  }

  implicit def qnameExporterQName: Exporter[sa.QName, DT#QName] =
    Exporter { case sa.QName(pfx, ln) =>
      QName(null.asInstanceOf[DT#Namespace], LocalName(ln.name), Prefix(pfx.pfx))
    }
}