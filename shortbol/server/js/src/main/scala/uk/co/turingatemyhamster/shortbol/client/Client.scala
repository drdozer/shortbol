package uk.co.turingatemyhamster.shortbol
package client

import java.io.StringWriter

import fastparse.core.Parsed.Success
import org.widok._
import org.widok.html._
import pl.metastack.metarx.Buffer
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ast.SBFile
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops.{Exporter, LogMessage, PrettyPrinter, ShortbolParser}

import scala.scalajs.js


/**
  *
  *
  * @author Matthew Pocock
  */
object Client extends PageApplication {
  implicit val ec = scala.scalajs.concurrent.JSExecutionContext.queue

  lazy val logMsgs: Buffer[LogMessage] = Buffer()

  override def view() = div(
    h1("Shortbol sandpit"),
    div(
      """@import stdlib:sbol
        |
        |@prefix test <http://me.name/test#>
        |@defaultPrefix test
        |
        |seq : DnaSequence("agct")
        |
        |cmp : DnaComponent
        |
        |prom : Promoter
        |""".stripMargin).id("shortbol_editor").css("shortbol_ace"),
    div().id("longbol").css("shortbol_ace"),
    div().id("xml-rdf").css("shortbol_ace"),
    div(logMsgs map { m => div(m.pretty).css(m.level.pretty) }).id("log")
    )

  override def ready(): Unit = {
    lazy val ace = js.Dynamic.global.ace

    println("Loading editor")
    val editor = ace.edit("shortbol_editor")
    println("Editor created")
    editor.setTheme("ace/theme/solarized_light")
    println("Set theme")
    editor.getSession().setMode("ace/mode/sbolshorthand2")
    println("Set mode")

    val result = ace.edit("longbol")
    result.setTheme("ace/theme/solarized_light")
    result.getSession().setMode("ace/mode/sbolshorthand2")
    result.setReadOnly(true)

    val xmlRdf = js.Dynamic.global.ace.edit("xml-rdf")
    xmlRdf.setTheme("ace/theme/solarized_light")
    xmlRdf.getSession().setMode("ace/mode/xml")
    xmlRdf.setReadOnly(true)

    editor.getSession().on("change", (e: AnyVal) => swallowExceptions(processShortbol()))
    processShortbol()

    def processShortbol(): Unit = {
      val txt = editor.getSession().getValue().asInstanceOf[String]

      ShortbolParser.SBFile.withPositions(ast.LocalName("shortbol_editor"), txt) match {
        case s: Success[SBFile] =>
          val (c, v) = s.value.eval.run(Fixture.configuredContext)

//          println("Setting log messages")
          logMsgs.clear()
          logMsgs ++= c.logms

          val sw = new StringWriter()
          PrettyPrinter(sw)(ast.SBFile(v))
          sw.append("\n")
//          println("Evaluated to:")
//          println(ast.SBFile(v))
//          println("Pretty printed as:")
//          println(sw)

          result.getSession().setValue(sw.toString)

//          println("Exporting to datatree")
          val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)
//          println("Exporting to xml-rdf")
          val rdfIo = RdfIo.rdfIo[datatree.ast.AstDatatree]
          val xml = RdfIo.write[datatree.ast.AstDatatree](doc)

          xmlRdf.getSession().setValue(xml.render(2))
      }
    }
  }

  def swallowExceptions[T](t: => T): Unit = try {
    t
  } catch {
    case e : Exception =>
      e.printStackTrace()
  }

}
