package uk.co.turingatemyhamster.shortbol
package pragma

import ast._
import ast.sugar._
import ops._
import ops.Eval._
import ShortbolParser.POps

import fastparse.core.Parsed.{Failure, Success}

import scala.io.Source
import scalaz.Scalaz._
import scalaz._

/**
  * Created by nmrp3 on 22/06/16.
  */
object ImportPragma {
  def apply(resolver: Resolver): Hook = new Hook {

    override def register(p: Pragma) = for {
      _ <- withPHooks(resolveImport)
    } yield List(p)

    def resolveImport(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("import"), args) =>
        args match {
          case Seq(ValueExp.Identifier(id)) =>
            resolver.resolve(id) match {
              case \/-(imported) => for {
                _ <- log(LogMessage.info(s"Importing $id", p.region))
                _ <- imported.eval
              } yield List(p)
              case -\/(err) =>
                for {
                  _ <- log(LogMessage.error(s"Import failed for $id", id.region, Some(err)))
                } yield Nil
            }
          case _ =>
            for {
              _ <- log(LogMessage.error(s"Malformed @import declaration", p.region))
            } yield Nil
        }
      case _ =>
        constant(List(p))
    }

    override val ID: LocalName = "import"

    override val bootstrap: String = "@pragma import url"
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
        case Url(u) =>
          val src = Source.fromURL(u)
          ShortbolParser.SBFile.withPositions(id, src.mkString) match {
            case Success(s, _) =>
              s.right
            case f: Failure =>
              new Exception(s"Failed to parse $u at ${f.index}: ${f.extra.traced}").left
          }
        case _ =>
          // todo: resolve non-url identifiers using the context
          new Exception(s"Could not resolve identifier $id").left
      }
    }
  }
}
