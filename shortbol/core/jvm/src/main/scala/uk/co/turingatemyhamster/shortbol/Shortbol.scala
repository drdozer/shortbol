package uk.co.turingatemyhamster
package shortbol

import java.io.{File, FileWriter, PrintWriter}
import javax.xml.stream.XMLOutputFactory

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.shortbol.ops._
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.sharedAst.sugar._

import scala.util.Try
import scopt.OptionParser
import uk.co.turingatemyhamster.datatree.io.RdfIo

import scala.io.Source

/**
  * Created by nmrp3 on 16/06/15.
  */
object Shortbol {

  val parser = new OptionParser[Config]("shortbol") {
    head("shortbol", "0.1")

    opt[Boolean]("exportLonghand")
      .action((e, c) => c.copy(exportLonghand = e))
      .text("export the longhand form, after template expansion")
    opt[Boolean]("exportFixedup")
      .action((e, c) => c.copy(exportFixedup = e))
      .text("export the longhand reperesentation, after fixup rules are applied")
    opt[Boolean]("exportRdf")
      .action((e, c) => c.copy(exportRdf = e))
      .text("export the SBOl rdf representation")
    opt[Boolean]("exportLog")
      .action((e, c) => c.copy(exportLog = e))
      .text("export logs")
    arg[File]("<file>...")
      .unbounded()
      .required()
      .action((f, c) => c.copy(inFiles = c.inFiles :+ f))
  }

  def main(args: Array[String]): Unit = {
    for {
      c <- parser.parse(args, Config()).to[List]
      inFile <- c.inFiles
    } yield {
      ShortbolParser.SBFile.withPositions(inFile.getAbsolutePath, Source.fromFile(inFile).mkString) match {
        case s: Success[shorthandAst.SBFile] =>
          val (evalCtxt, evaluated) = s.value.eval.run(Fixture.configuredContext)
          val (fuCtxt, fixedUp) =
            Fixture.doFixup(evaluated).run(evalCtxt)

          if(c.exportLonghand) exportLonghand(c, inFile, evaluated)
          if(c.exportFixedup) exportFixedup(c, inFile, fixedUp)
          if(c.exportRdf) exportRdf(c, inFile, fuCtxt, fixedUp)
          if(c.exportLog) exportLog(c, inFile, fuCtxt.logms)
        case e: Failure =>
          System.err.println(s"Problem parsing $inFile")
      }
    }
  }

  def rewriteExtension(file: File, oldExt: String, newExt: String) =
    if(file.getName.endsWith(oldExt))
      new File(file.getCanonicalPath.replaceFirst(s"""\\.$oldExt$$""", s".$newExt"))
    else
      new File(s"${file.getCanonicalPath}.$newExt")

  def exportLonghand(c: Config, shorthandFile: File, sf: longhandAst.SBFile): Try[File] = Try {
    val longhandFile = rewriteExtension(shorthandFile, c.shortbolExtn, c.longhandExtn)
    val w = new FileWriter(longhandFile)
    PrettyPrinter(w)(sf)
    w.close()
    longhandFile
  }

  def exportFixedup(c: Config, shorthandFile: File, sf: longhandAst.SBFile): Try[File] = Try {
    val fixedupFile = rewriteExtension(shorthandFile, c.shortbolExtn, c.fixedupExtn)
    val w = new FileWriter(fixedupFile)
    PrettyPrinter(w)(sf)
    w.close()
    fixedupFile
  }

  def exportRdf(c: Config, shorthandFile: File, ctxt: EvalContext, sf: longhandAst.SBFile): Try[File] = Try {
    val rdfFile = rewriteExtension(shorthandFile, c.shortbolExtn, c.rdfExtn)
    val doc = Exporter[datatree.ast.AstDatatree](ctxt).apply(sf)
    val rdfIo = implicitly[RdfIo[datatree.ast.AstDatatree]]
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        new FileWriter(rdfFile)))
    rdfIo.write(xmlWriter, doc)
    xmlWriter.close()
    rdfFile
  }

  def exportLog(c: Config, shorthandFile: File, log: List[LogMessage]): Try[File] = Try {
    val logFile = rewriteExtension(shorthandFile, c.shortbolExtn, c.logExtn)
    val w = new PrintWriter(new FileWriter(logFile))
    for(l <- log) w println l.pretty
    w.close()
    logFile
  }
}

case class Config(
                   shortbolExtn: String = "sbol",
                   longhandExtn: String = "lbol",
                   fixedupExtn: String = "fbol",
                   rdfExtn: String = "xml",
                   logExtn: String = "log",
                   inFiles: List[File] = Nil,
                   exportLonghand: Boolean = false,
                   exportFixedup: Boolean = false,
                   exportRdf: Boolean = true,
                   exportLog: Boolean = false
                 )