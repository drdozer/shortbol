package uk.turingatemyhamster.shortbol.server

import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.http.scaladsl.server
import akka.stream.ActorFlowMaterializer
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import fastparse.core.Result.{Failure, Success}
import uk.co.turingatemyhamster.shortbol._

import scala.concurrent.duration._

object ShortbolServer extends App{

  lazy val config = ConfigFactory.load()


  implicit val system = ActorSystem("actor-system", config)
  implicit val materializer = ActorFlowMaterializer()
  implicit val executor = system.dispatcher
  implicit val timeout: Timeout = Timeout(5 seconds)

  import server._
  import model._
  import Directives._

  val routes: Route = {
    pathPrefix("shortbol") {
      path("expand") {
        post {
          entity(as[String]) { text =>
            complete {
              ShortbolParser.parser.File.parse(text) match {
                case Success(tls, _) =>
                  val out = new java.lang.StringBuilder
                  val pp = PrettyPrinter(out)
                  val cstrs = Ops.constructors(tls)
                  val inds = tls collect { case i : InstanceExp => i }
                  val ex = ExpansionContext(cstrs, Bindings(Map()))

                  import Expander.ops._

                  for {
                    i <- inds
                    ie <- i expandWith ex
                  } {
                    pp.append(ie)
                  }

                  out.toString()
                case f : Failure =>
                  f.verboseTrace
              }
            }
          }
        }
      }
    }
  }
}