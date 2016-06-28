package uk.co.turingatemyhamster.shortbol

import java.io.{File, FileWriter}
import javax.xml.stream.XMLOutputFactory

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ast.sugar._
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops.{Exporter, PrettyPrinter, ShortbolParser}

import scala.io.Source

/**
  *
  *
  * @author Matthew Pocock
  */
object ExportShortbol {
  def main(args: Array[String]): Unit = for(file <- args) {
    println(s"Processing file $file")
    val f = new File(file)
    if(f.exists()) {
      ShortbolParser.SBFile.withPositions(file, Source.fromFile(new File(file)).mkString) match {
        case s : Success[ast.SBFile] =>
          println("Loaded:")
          PrettyPrinter(System.out)(s.value)
          println
          println
          val (c, v) = s.value.eval.run(Fixture.configuredContext)
          println("Expanded to:")
          PrettyPrinter(System.out)(ast.SBFile(v))
          println
          println
          println("With logs:")
          println(c.logms.map(_.pretty).mkString("\n"))
          val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)
          val rdfIo = implicitly[RdfIo[datatree.ast.AstDatatree]]
          val xmlWriter = new IndentingXMLStreamWriter(
            XMLOutputFactory.newInstance.createXMLStreamWriter(
              new FileWriter(s"$file.rdf")))
          rdfIo.write(xmlWriter, doc)
        case e : Failure =>
          System.err.println(s"Problem parsing $file")
      }
    } else {
      System.err.println(s"File $file doesn't exist")
    }
  }
}
