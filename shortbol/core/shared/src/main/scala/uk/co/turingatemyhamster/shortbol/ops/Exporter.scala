package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import web._

/**
 * Created by nmrp3 on 23/09/15.
 */
//case class Exporter[T, E](export: T => E)
//
//object Exporter {
//
//  implicit class ExporterOps[T](val _t: T) extends AnyVal {
//    def export[E](implicit ev: Exporter[T, E]): E = ev.export(_t)
//  }
//
//}
//
//object ExporterEnv {
//  def apply[DT <: Datatree](implicit _dtDSL: DatatreeDSL[DT], _webDSL: WebDSL[DT], _relDSL: RelationsDSL[DT]) = new ExporterEnv[DT] {
//    val dtDSL = _dtDSL
//    val webDSL = _webDSL
//    val relDSL = _relDSL
//  }
//}
//
//trait ExporterEnv[DT <: Datatree] {
//  implicit val dtDSL: DatatreeDSL[DT]
//  implicit val webDSL: WebDSL[DT]
//  implicit val relDSL: RelationsDSL[DT]
//
//  import dtDSL._
//  import webDSL._
//  import relDSL._
//
//  import Exporter._
//
//  implicit def seqExporter[T, E](implicit ev: Exporter[T, E]): Exporter[Seq[T], Seq[E]] =
//    Exporter { _ map ev.export }
//
//
//
//  implicit val sbfileExporter: Exporter[SBFile, DT#DocumentRoot] =
//    Exporter { (t: SBFile) =>
//      DocumentRoot(ZeroMany(), ZeroMany(t.tops.export.flatten :_*))
//    }
//
//
//
//  implicit val instanceExpExporter: Exporter[InstanceExp, DT#TopLevelDocument] =
//    Exporter { case (t: InstanceExp) =>
//      TopLevelDocument(ZeroMany(), ZeroOne(t.id.export), One(t.cstrApp.cstr.export), ZeroMany())
//  }
//
//  implicit val tpeConstructor: Exporter[TpeConstructor, DT#QName] =
//    Exporter[TpeConstructor, DT#QName] { _.id.export }
//
//  implicit val urlExporter: Exporter[Url, DT#Uri] =
//    Exporter { (u: Url) =>
//      u match {
//        case shortbol.Url(ln) =>
//          Uri(ln)
//      }
//  }
//
//  implicit def qnameExporterQName: Exporter[QName, DT#QName] =
//    Exporter { case shortbol.QName(pfx, ln) =>
//      QName(null.asInstanceOf[DT#Namespace], LocalName(ln.name), Prefix(pfx.pfx))
//    }
//}