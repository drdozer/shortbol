package uk.co.turingatemyhamster.shortbol

import java.io.{File, FileWriter}
import javax.xml.stream.XMLOutputFactory

import uk.co.turingatemyhamster.shortbol.ops.{Exporter, ShortbolParser}
import ShortbolParser.POps
import ast.sugar._
import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import ops.Eval.EvalOps
import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo

import scala.io.Source

/**
  *
  *
  * @author Matthew Pocock
  */
object ExportShortbol {
  def main(args: Array[String]): Unit = for(file <- args) {
    ShortbolParser.SBFile.withPositions(file, Source.fromFile(new File(file)).mkString) match {
      case s : Success[ast.SBFile] =>
        val (c, v) = s.value.eval.run(Fixture.configuredContext)
        val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)
        val rdfIo = implicitly[RdfIo[datatree.ast.AstDatatree]]
        val xmlWriter = new IndentingXMLStreamWriter(
          XMLOutputFactory.newInstance.createXMLStreamWriter(
            new FileWriter(s"$file.rdf")))
        rdfIo.write(xmlWriter, doc)
      case e : Failure =>
        System.err.println(s"Problem parsing $file")
    }
  }
}
