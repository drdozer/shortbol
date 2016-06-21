package uk.co.turingatemyhamster.shortbol
package server


import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import fastparse.core.Parsed.{Failure, Success}

import scala.io.StdIn
import ast._
import com.typesafe.config.ConfigFactory
import ops._


/**
 *
 *
 * @author Matthew Pocock
 */
object ShortbolServer {

  def main(args: Array[String]): Unit = {
    lazy val config = ConfigFactory.load()

    implicit val system = ActorSystem("actor-system", config)
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val routes: Route = {
      pathPrefix("shortbol") {
        path("expand") {
          post {
            entity(as[String]) { text =>
              complete {
                ShortbolParser.SBFile.parse(text) match {
                  case s: Success[SBFile] =>
                    val out = new java.lang.StringBuilder
                    val pp = PrettyPrinter(out)
                    pp(s.value)
                      out.toString()
                  case f: Failure =>
                    f.extra.traced.trace
                }
              }
            }
          }
        }
      }
    }
  }
}