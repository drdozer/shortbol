package uk.co.turingatemyhamster.shortbol
package pragma

import ast._
import ast.sugar._
import ops._
import ops.Eval._

import fastparse.core.Parsed.{Failure, Success}

import scala.io.Source
import scalaz.Scalaz._
import scalaz._

/**
  * Created by nmrp3 on 22/06/16.
  */
object Import {
  def apply(resolver: Resolver): Hook = new Hook {

    override def register(p: Pragma) =
      modify((_: EvalContext).withPHooks(resolveImport))

    def resolveImport(p: Pragma): EvalState[Unit] = p match {
      case ast.Pragma(LocalName("import"), Seq(ValueExp.Identifier(url))) =>
        resolver.resolve(url) match {
          case \/-(imported) => for {
            _ <- imported.eval
          } yield ()
          case -\/(err) =>
            modify((_: EvalContext).withThrown(err))
        }
      case _ =>
        constant(())
    }
  }
}

trait Resolver {
  def resolve(id: Identifier): Throwable \/ SBFile
}

object Resolver {
  def fromValues(vs: (Identifier, SBFile)*): Resolver = new Resolver {
    val id2F = Map(vs :_*)

    override def resolve(id: Identifier): Throwable \/ SBFile =
      id2F get id match {
        case Some(f) => f.right
        case None => new NoSuchElementException(s"Unable to resolve shortbol for $id").left
      }
  }

  def fromWeb: Resolver = new Resolver {
    override def resolve(id: Identifier): Throwable \/ SBFile = {
      id match {
        case Url(url) =>
          val src = Source.fromURL(url)
          ShortbolParser.SBFile.parse(src.mkString) match {
            case Success(s, _) =>
              s.right
            case f: Failure =>
              new Exception(s"Failed to parse $url at ${f.index}: ${f.extra.traced}").left
          }
        case _ =>
          // todo: resolve non-url identifiers using the context
          new Exception(s"Could not resolve identifier $id").left
      }
    }
  }
}
