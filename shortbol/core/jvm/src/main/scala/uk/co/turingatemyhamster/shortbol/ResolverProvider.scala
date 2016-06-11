package uk.co.turingatemyhamster.shortbol

import java.net.{URI}
import fastparse.core.Parsed.{Success, Failure}

import scala.io.Source
import scalaz._
import Scalaz._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait ResolverProvider extends ResolverBase {

  protected def parser: ShortbolParser.type

  override def resolve(baseUrl: Option[Url], url: Url): Throwable \/ SBFile = {
    val resUri = baseUrl match {
      case Some(b) =>
        new URI(b.url).resolve(url.url).toString
      case None =>
        url.url
    }
    val src = Source.fromURL(resUri)
    parser.SBFile.parse(src.mkString) match {
      case Success(s, _) =>
        s.copy(rdfAbout = Some(url), source = Some(Url(resUri))).right
      case f: Failure =>
        new Exception(s"Failed to parse $url at ${f.index}: ${f.extra.traced}").left
    }
  }
}
