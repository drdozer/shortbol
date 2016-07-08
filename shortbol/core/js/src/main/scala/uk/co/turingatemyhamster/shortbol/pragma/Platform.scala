package uk.co.turingatemyhamster.shortbol.pragma

import java.io.IOException

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
    if(xmlHttp.status == 200)
      xmlHttp.responseText
    else
      throw new IOException(s"Failed to slurp $url with status ${xmlHttp.status}")
  }
}
