package uk.co.turingatemyhamster.shortbol

import java.io.StringWriter
import javax.xml.stream.XMLOutputFactory

import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ast.sugar._
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops._
import utest._

/**
  *
  *
  * @author Matthew Pocock
  */
object ExporterTestSuite extends TestSuite {

  val xof = XMLOutputFactory.newInstance()

  def process(sbol: String) = {
    val (c, s) = ShortbolParser.SBFile.withPositions("_test_", sbol).get.value.eval.run(Fixture.configuredContext)

    val doc = Exporter[datatree.ast.AstDatatree](c).apply(s)

    val rdfIo = RdfIo.rdfIo[datatree.ast.AstDatatree]
    val xmlText = new StringWriter()
    val xtw = xof.createXMLStreamWriter(xmlText)
    RdfIo.write[datatree.ast.AstDatatree](xtw, doc)

    xmlText.toString
  }

  override def tests = TestSuite {
    'noAbout - {
      process(
        """@prefix rdf <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |@prefix foaf <http://xmlns.com/foaf/0.1/>
          |@prefix test <http://test.com/myTest/>
          |
          |test:matthew : foaf:Person
        """.stripMargin)
    }

    'withAbout - {
      process(
        """@prefix rdf <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |@prefix foaf <http://xmlns.com/foaf/0.1/>
          |@prefix test <http://test.com/myTest/>
          |
          |test:matthew : foaf:Person
          |  rdf:about = <foo.bar.com/matt1234>
        """.stripMargin)
    }
  }
}
