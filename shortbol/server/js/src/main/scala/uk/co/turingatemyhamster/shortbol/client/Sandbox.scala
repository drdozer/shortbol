package uk.co.turingatemyhamster.shortbol
package client

import java.io.StringWriter

import fastparse.core.Parsed.Success
import org.widok._
import org.widok.html._
import pl.metastack.metarx.Buffer
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.ast.AstDatatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.shorthandAst.SBFile
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.{RepairComponents, RepairIdentities}
import uk.co.turingatemyhamster.shortbol.ops._

import scala.scalajs.js


/**
  *
  *
  * @author Matthew Pocock
  */
object Sandbox extends PageApplication {
  implicit val ec = scala.scalajs.concurrent.JSExecutionContext.queue

  lazy val logMsgs: Buffer[LogMessage] = Buffer()

  override def view() = div(
    h1("Shortbol sandpit"),
    h2("Editor"),
    div(
      """@import stdlib:sbol
        |
        |@prefix test <http://me.name/test#>
        |@defaultPrefix test
        |
        |seq : DnaSequence("agct")
        |
        |cmp : DnaComponent
        |  sequence = seq
        |
        |prom : Promoter
        |  displayId = "p1"
        |  name = "Promoter 1"
        |  description = "The first promoter"
        |""".stripMargin).id("shortbol_editor").css("shortbol_ace"),
    h2("Template expansion"),
    div().id("longbol").css("shortbol_ace"),
    h2("Fixed up"),
    div().id("fixedbol").css("shortbol_ace"),
    h2("SBOL XML/RDF"),
    div().id("xml-rdf").css("shortbol_ace"),
    h2("Log messages"),
    div(logMsgs map { m => div(m.pretty).css(m.level.pretty) }).id("log")
    )

  override def ready(): Unit = {
    lazy val ace = js.Dynamic.global.ace
    lazy val rdf = js.Dynamic.global.rdf
    lazy val rdf_xml_parser = js.Dynamic.global.rdf

    println("Loading editor")
    val editor = ace.edit("shortbol_editor")
    println("Editor created")
    editor.setTheme("ace/theme/solarized_light")
    println("Set theme")
    editor.getSession().setMode("ace/mode/sbolshorthand2")
    println("Set mode")

    val longbol = ace.edit("longbol")
    longbol.setTheme("ace/theme/solarized_light")
    longbol.getSession().setMode("ace/mode/sbolshorthand2")
    longbol.setReadOnly(true)

    val fixedbol = ace.edit("fixedbol")
    fixedbol.setTheme("ace/theme/solarized_light")
    fixedbol.getSession().setMode("ace/mode/sbolshorthand2")
    fixedbol.setReadOnly(true)

    val xmlRdf = js.Dynamic.global.ace.edit("xml-rdf")
    xmlRdf.setTheme("ace/theme/solarized_light")
    xmlRdf.getSession().setMode("ace/mode/xml")
    xmlRdf.setReadOnly(true)

    editor.getSession().on("change", (e: AnyVal) => swallowExceptions(processShortbol()))
    processShortbol()

    def processShortbol(): Unit = {
      val txt = editor.getSession().getValue().asInstanceOf[String]

      ShortbolParser.SBFile.withPositions(shorthandAst.LocalName("shortbol_editor"), txt) match {
        case s: Success[SBFile] =>
          val (c1, v1) = s.value.eval.run(Fixture.configuredContext)

//          println("Setting log messages")

          def stringify(sf: longhandAst.SBFile) = {
            val sw = new StringWriter()
            PrettyPrinter(sw)(sf)
            sw.append("\n")
            sw.toString
            sw
          }

          longbol.getSession().setValue(stringify(v1).toString)

          val (c2, v2) = Fixture.doFixup(v1).run(c1)

          fixedbol.getSession().setValue(stringify(v2).toString)


          logMsgs.clear()
          logMsgs ++= c2.logms

          //          println("Exporting to datatree")
          val doc = Exporter[AstDatatree](c2).apply(v2)
          //          println("Exporting to xml-rdf")
          val rdfIo = RdfIo.rdfIo[AstDatatree]
          val xml = RdfIo.write[AstDatatree](doc)
          val xmlText = xml.render(2)

          xmlRdf.getSession().setValue(xmlText)
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
