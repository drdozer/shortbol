package uk.co.turingatemyhamster.shortbol

import scalaz._
import Scalaz._

trait Resolver {
  def resolve(id: Identifier): Throwable \/ SBFile
}

object Resolver {
  def fromValues(vs: (Identifier, SBFile)*): Resolver = new Resolver {
    val id2F = Map(vs :_*)

    override def resolve(id: Identifier): Disjunction[Throwable, SBFile] =
      id2F get id match {
        case Some(f) => f.right
        case None => new NoSuchElementException(s"Unable to resolve shortbol for $id").left
      }
  }
}