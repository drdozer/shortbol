package uk.co.turingatemyhamster.shortbol.pragma

import org.scalajs.dom.raw.XMLHttpRequest

/**
  *
  *
  * @author Matthew Pocock
  */
object Platform {
  def slurp(url: String): String = {
    val xmlHttp = new XMLHttpRequest
    xmlHttp.open("GET", url, false)
    xmlHttp.send(null)
    xmlHttp.responseText
  }
}
