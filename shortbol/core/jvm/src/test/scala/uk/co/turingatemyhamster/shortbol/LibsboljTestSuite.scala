package uk.co.turingatemyhamster.shortbol

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import ops._
import ast.sugar._
import ShortbolParser.POps
import Eval.EvalOps
import utest._

import scalaz._
import Scalaz._
import org.sbolstandard.core2.{SBOLDocument, SBOLFactory}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo

/**
  * Created by nmrp3 on 22/07/16.
  */
object LibsboljTestSuite extends TestSuite {
  def toLibSBOLj(shortbol: String) = {
    val sb = ShortbolParser.SBFile.withPositions("_test_", shortbol).get.value
    val (c, v) = sb.eval.run(Fixture.configuredContext)
    val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)
    //          println("Exporting to xml-rdf")
    val rdfIo = RdfIo.rdfIo[datatree.ast.AstDatatree]
    val xml = RdfIo.write[datatree.ast.AstDatatree](doc)
    val xmlText = xml.render(2)

    val sbolDoc = new SBOLDocument()
    SBOLFactory.setSBOLDocument(sbolDoc)
    SBOLFactory.read(new ByteArrayInputStream(xmlText.getBytes(StandardCharsets.UTF_8)))

    sbolDoc
  }

  override def tests = TestSuite {
    'empty - {
      toLibSBOLj(
        """
          |
        """.stripMargin)
    }

    'fail - {
      assert(false)
    }
  }
}
