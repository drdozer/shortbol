package uk.co.turingatemyhamster.shortbol

import java.net.URL
import fastparse.core.Result

import scala.io.Source
import scalaz._
import Scalaz._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait ResolverProvider extends Resolver {

  protected def parser: ShortbolParser.type

  override def resolve(id: Identifier): Throwable \/ SBFile = id match {
    case Url(url) =>
      val src = Source.fromURL(url)
      parser.SBFile.parse(src.mkString) match {
        case Result.Success(s, _) => s.right
        case f : Result.Failure =>
          new Exception(s"Failed to parse $url at ${f.index}: ${f.traced.fullStack}").left
      }
    case _ =>
      (new IllegalArgumentException(s"Expecting a URL but got $id")).left
  }

}
