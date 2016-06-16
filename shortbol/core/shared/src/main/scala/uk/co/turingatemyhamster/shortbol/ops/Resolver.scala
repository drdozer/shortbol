package uk.co.turingatemyhamster.shortbol.ops

import uk.co.turingatemyhamster.shortbol.ast._

import scalaz.Scalaz._
import scalaz._

trait Resolver {
  def resolve(ctxt: ResolutionContext, id: Identifier): Throwable \/ SBFile
}

object Resolver {
  def fromValues(vs: (Identifier, SBFile)*): Resolver = new Resolver {
    val id2F = Map(vs :_*)

    override def resolve(ctxt: ResolutionContext, id: Identifier): Throwable \/ SBFile =
      id2F get id match {
        case Some(f) => f.right
        case None => new NoSuchElementException(s"Unable to resolve shortbol for $id").left
      }
  }
}

trait ResolverBase extends Resolver {
  override def resolve(ctxt: ResolutionContext, id: Identifier): Throwable \/ SBFile = {
    val localUrl = id match {
      case url : Url =>
        Some(url)
      case qname: QName =>
        ctxt.bindings.resolve(Some(qname.prefix), qname.localName)
      case lname: LocalName =>
        ctxt.bindings.resolve(None, lname)
    }

    localUrl match {
      case Some(url) =>
        resolve(ctxt.baseUrl, url)
      case None =>
        new NoSuchElementException(s"Could not resolve $id").left
    }
  }

  def resolve(baseUrl: Option[Url], url: Url): Throwable \/ SBFile
}

case class ResolutionContext(baseUrl: Option[Url], bindings: PrefixBindings)

trait PrefixBindings {
  def resolve(pfx: Option[NSPrefix]): Option[Url]

  final def resolve(pfx: Option[NSPrefix], lName: LocalName): Option[Url] =
    for {
      pfxU <- resolve(pfx)
    } yield Url(s"${pfxU.url}${lName.name}")
}

case class PrefixBindingsImpl(anonymous: Option[Url], named: Map[NSPrefix, Url]) extends PrefixBindings {
  override def resolve(pfx: Option[NSPrefix]): Option[Url] = pfx match {
    case None =>
      anonymous
    case Some(p) =>
      named get p
  }
}