package uk.co.turingatemyhamster.shortbol

import java.net.URI

import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.shortbol.ast.{SBFile, Url}
import uk.co.turingatemyhamster.shortbol.ops.{ResolverBase, ShortbolParser}

import scalaz._
import Scalaz._
import scala.io.Source

/**
  *
  *
  * @author Matthew Pocock
  */
trait ResolverProvider extends ResolverBase {
  override def resolve(baseUrl: Option[Url], url: Url): Throwable \/ SBFile = {
    val resUri = baseUrl match {
      case Some(b) =>
        new URI(b.url).resolve(url.url).toString
      case None =>
        url.url
    }
    val src = Source.fromURL(resUri)
    ShortbolParser.SBFile.parse(src.mkString) match {
      case Success(s, _) =>
        s.copy(rdfAbout = Some(url), source = Some(Url(resUri))).right
      case f: Failure =>
        new Exception(s"Failed to parse $url at ${f.index}: ${f.extra.traced}").left
    }
  }
}

