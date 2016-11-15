package uk.co.turingatemyhamster.shortbol
package server


import java.io.{FileWriter, StringWriter}
import javax.xml.stream.XMLOutputFactory

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import fastparse.core.Parsed.{Failure, Success}
import akka.http.scaladsl.marshalling.Marshaller
import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import com.typesafe.config.ConfigFactory
import ops._
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.{RepairComponents, RepairIdentities}
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps


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

    implicit val sbMarshaller = Marshaller.StringMarshaller.compose { (ctxt_sf : (EvalContext, longhandAst.SBFile)) =>
      val (ctxt, sf) = ctxt_sf
      val doc = Exporter[datatree.ast.AstDatatree](ctxt).apply(sf)
      val rdfIo = implicitly[RdfIo[datatree.ast.AstDatatree]]
      val res = new StringWriter()
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          res))
      rdfIo.write(xmlWriter, doc)
      xmlWriter.close()
      res.toString
    }

    val routes: Route = {
      pathPrefix("shortbol") {
        path("expand") {
          post {
            entity(as[String]) { text =>
              complete {
                ShortbolParser.SBFile.parse(text) match {
                  case s: Success[shorthandAst.SBFile] =>
                    val (evalCtxt, evaluated) = s.value.eval.run(Fixture.configuredContext)
                    Fixture.doFixup(evaluated).run(evalCtxt)
                  case f: Failure =>
                    throw new IllegalArgumentException(f.extra.traced.trace)
                }
              }
            }
          }
        }
      } ~
      get {
        getFromResourceDirectory("")
      }
    }

    val port = 10080
    val bindingsFuture = Http().bindAndHandle(routes, "0.0.0.0", port)
    println(s"Service deployed to http://localhost:$port/")
  }
}