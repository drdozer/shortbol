package uk.co.turingatemyhamster.shortbol.pragma

import scala.io.Source

/**
  *
  *
  * @author Matthew Pocock
  */
object Platform {
  def slurp(url: String): String = Source.fromURL(url).mkString
}
