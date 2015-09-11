package uk.co.turingatemyhamster.shortbol

import java.net.{URI}
import fastparse.core.Result

import scala.io.Source
import scalaz._
import Scalaz._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait ResolverProvider extends ResolverBase {

  protected def parser: ShortbolParser.type

  override def resolve(baseUrl: Url, url: Url): Throwable \/ SBFile = {
    val resUri = new URI(baseUrl.url).resolve(url.url)
    val src = Source.fromURL(resUri.toString)
    parser.SBFile.parse(src.mkString) match {
      case Result.Success(s, _) => s.right
      case f: Result.Failure =>
        new Exception(s"Failed to parse $url at ${f.index}: ${f.traced.fullStack}").left
    }
  }
}
