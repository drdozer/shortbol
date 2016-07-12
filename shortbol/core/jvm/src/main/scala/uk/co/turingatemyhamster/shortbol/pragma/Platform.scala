package uk.co.turingatemyhamster.shortbol.pragma

import java.io.FileNotFoundException

import scala.io.Source

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object Platform {
  def slurp(url: String): Throwable \/ String =
    try {
      Source.fromURL(url).mkString.right
    } catch {
      case e1 : FileNotFoundException =>
        try {
          Source.fromURL(url ++ ".sbol").mkString.right
        } catch {
          case e2 : FileNotFoundException =>
            e1.left
        }
    }
}
