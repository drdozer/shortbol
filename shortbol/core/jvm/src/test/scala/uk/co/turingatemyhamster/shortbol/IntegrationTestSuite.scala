package uk.co.turingatemyhamster.shortbol

import utest._

import uk.co.turingatemyhamster.shortbol.EvalTestSuite._
import ast._

object IntegrationTestSuite extends TestSuite {
  override def tests = TestSuite {
    * - {
      parse("""@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol.sbol>
              |p : Promoter""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """p : sbol:ComponentDefinition
            |  sbol:type = sbol:DNA
            |  sbol:role = sbol:Promoter""".stripMargin) in ⊥
    }

    * - {
      parse("""@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol.sbol>
              |
              |@prefix test <http://me.name/test#>
              |@defaultPrefix test
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """test:seq : sbol:Sequence
            |  sbol:encoding = <http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html>
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    * - {
      parse("""@prefix sbol <http://sbols.org/v2#>
              |@defaultPrefix sbol
              |
              |DnaSequence(x) => Sequence
              |  encoding = IUPACDNA
              |  elements = x
              |
              |@prefix test <http://me.name/test#>
              |@defaultPrefix test
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """test:seq : sbol:Sequence
            |  sbol:encoding = sbol:IUPACDNA
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    * - {
      parse("""@prefix sbol <http://sbols.org/v2#>
              |@defaultPrefix sbol
              |
              |DnaSequence(x) => Sequence
              |  encoding = iupac:DNA
              |  elements = x
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """sbol:seq : sbol:Sequence
            |  sbol:encoding = iupac:DNA
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    * - {
      parse("""DnaSequence(x) => Sequence
              |  encoding = IUPACDNA
              |  elements = x
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """seq : Sequence
            |  encoding = IUPACDNA
            |  elements = "agct"""".stripMargin) in ⊥

    }
  }
}
