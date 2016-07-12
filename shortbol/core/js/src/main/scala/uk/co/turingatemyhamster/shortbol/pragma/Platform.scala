package uk.co.turingatemyhamster.shortbol.pragma

import java.io.IOException

import org.scalajs.dom.raw.XMLHttpRequest

import scalaz._
import scalaz.Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object Platform {
  def slurp(url: String): Throwable \/ String = {
    val xmlHttp = new XMLHttpRequest
    println("Made request")
    try {
      xmlHttp.open("GET", url, false)
      println("Opened connection")
      xmlHttp.send(null)
      println("Sent null")
      if(xmlHttp.status == 200) {
        println("Returning response text")
        xmlHttp.responseText.right
      }
      else
      {
        println("Bad status. Raising exception.")
        (new IOException(s"Failed to slurp $url with status ${xmlHttp.status}")).left
      }
    } catch {
      case t : Throwable =>
        println("Caught exception - raising one")
        (new IOException(s"Failed to slurp $url", t)).left
    }
  }
}
