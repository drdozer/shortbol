package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import web._

/**
 * Created by nmrp3 on 23/09/15.
 */
trait Exporter[T, E] {
  def apply(t: T): E
}

object Exporter {
  implicit def export[T, E](t: T)(implicit ev: Exporter[T, E]): E = ev.apply(t)
  implicit def exportAs[E] = new AnyRef {
    def apply[T](t: T)(implicit ev: Exporter[T, E]): E = ev.apply(t)
  }

  implicit def seqExporter[T, E](implicit ev: Exporter[T, E]): Exporter[Seq[T], Seq[E]] = new Exporter[Seq[T], Seq[E]] {
    override def apply(t: Seq[T]): Seq[E] = t map ev.apply
  }

  implicit def sbfileExporter[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                              webDsl: WebDSL[DT],
                                              relDsl: RelationsDSL[DT]): Exporter[SBFile, DT#DocumentRoot] = new Exporter[SBFile, DT#DocumentRoot] {
    override def apply(t: SBFile): DT#DocumentRoot = {
      import dtDsl._
      import relDsl._

      DocumentRoot(ZeroMany(), ZeroMany(export(t.tops).flatten :_*))
    }
  }

  implicit def topLevelExporter[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                                webDsl: WebDSL[DT],
                                                relDsl: RelationsDSL[DT]): Exporter[TopLevel, Option[DT#TopLevelDocument]] = new Exporter[TopLevel, Option[DT#TopLevelDocument]] {
    override def apply(t: TopLevel): Option[DT#TopLevelDocument] = t match {
      case i : InstanceExp =>
        Some(export(i))
      case _ =>
        None
    }
  }

  implicit def instanceExpExporter[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                                   webDsl: WebDSL[DT],
                                                   relDsl: RelationsDSL[DT]): Exporter[InstanceExp, DT#TopLevelDocument] = new Exporter[InstanceExp, DT#TopLevelDocument] {
    override def apply(t: InstanceExp): DT#TopLevelDocument = {
      import dtDsl._
      import relDsl._

      TopLevelDocument(ZeroMany(), ZeroOne(exportAs[DT#Uri](t.id)), One(exportAs[DT#QName](t.cstrApp.cstr)), ZeroMany())
    }
  }

  implicit def tpeConstructor[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                              webDsl: WebDSL[DT],
                                              relDsl: RelationsDSL[DT]): Exporter[TpeConstructor, DT#QName] = new Exporter[TpeConstructor, DT#QName] {
    override def apply(t: TpeConstructor): DT#QName = t match {
      case TpeConstructor1(id, _) =>
        exportAs[DT#QName](id)
    }
  }

  implicit def identifierExporterUri[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                                     webDsl: WebDSL[DT],
                                                     relDsl: RelationsDSL[DT]): Exporter[Identifier, DT#Uri] = new Exporter[Identifier, DT#Uri] {
    override def apply(t: Identifier): DT#Uri = {
      import webDsl._

      t match {
        case shortbol.Url(ln) =>
          Uri(ln)
      }
    }
  }

  implicit def identifierExporterQName[DT <: Datatree](implicit dtDsl: DatatreeDSL[DT],
                                                     webDsl: WebDSL[DT],
                                                     relDsl: RelationsDSL[DT]): Exporter[Identifier, DT#QName] = new Exporter[Identifier, DT#QName] {
    override def apply(t: Identifier): DT#QName = {
      import webDsl._

      t match {
        case shortbol.QName(pfx, ln) =>
          QName(null.asInstanceOf[DT#Namespace], LocalName(ln.name), Prefix(pfx.pfx))
      }
    }
  }
}