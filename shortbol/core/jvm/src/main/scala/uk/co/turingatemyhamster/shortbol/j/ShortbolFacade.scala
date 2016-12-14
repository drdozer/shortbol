package uk.co.turingatemyhamster.shortbol.j

import java.io.{FileWriter, StringWriter}
import javax.xml.stream.XMLOutputFactory

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ops._
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.sharedAst.sugar._
import uk.co.turingatemyhamster.shortbol.{Fixture, shorthandAst}

/**
  * Created by nmrp3 on 14/12/16.
  */
class ShortbolFacade {
  def convertShortbolToSbol(shortbol: String): String =
    ShortbolParser.SBFile.withPositions("__fromJava__", shortbol)  match {
      case s: Success[shorthandAst.SBFile] =>
        val (evalCtxt, evaluated) = s.value.eval.run(Fixture.configuredContext)
        val (fuCtxt, fixedUp) =
          Fixture.doFixup(evaluated).run(evalCtxt)
        val doc = Exporter[datatree.ast.AstDatatree](fuCtxt).apply(fixedUp)
        val rdfIo = implicitly[RdfIo[datatree.ast.AstDatatree]]
        val rdfXml = new StringWriter()
        val xmlWriter = new IndentingXMLStreamWriter(
          XMLOutputFactory.newInstance.createXMLStreamWriter(rdfXml))
        rdfIo.write(xmlWriter, doc)
        xmlWriter.close()
        rdfXml.toString
      case e: Failure =>
        throw new IllegalArgumentException(s"Unable to parse shortbol: ${e.msg}")
    }
}
