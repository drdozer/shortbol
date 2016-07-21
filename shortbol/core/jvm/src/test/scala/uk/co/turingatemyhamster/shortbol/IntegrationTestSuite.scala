package uk.co.turingatemyhamster.shortbol

import utest._

import uk.co.turingatemyhamster.shortbol.EvalTestSuite._
import ast._

object IntegrationTestSuite extends TestSuite {
  override def tests = TestSuite {
    'dna_mapping - {
      import uk.co.turingatemyhamster.shortbol.ops.Eval._
      import ast.sugar._

      val ctxt =
        parse("@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol.sbol>").eval.exec(
          Fixture.configuredContext)
      println(s"With value bindings: ${ctxt.vlxps}")

      val res = ("iupac" :# "DNA").eval.eval(ctxt)
      assert(res == Url("http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html"))
    }

    'promoter_url - {
      parse("""@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol.sbol>
              |p : Promoter""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances_eval(
          """p : sbol:ComponentDefinition
            |  sbol:type = biopax:DnaRegion
            |  sbol:role = so:_0000167""".stripMargin) in ⊥
    }

    'dnaSequence_url - {
      parse("""@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol.sbol>
              |
              |@prefix test <http://me.name/test#>
              |@defaultPrefix test
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances_eval(
          """test:seq : sbol:Sequence
            |  sbol:encoding = <http://www.chem.qmul.ac.uk/iubmb/misc/naseq.html>
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    'two_prefixes - {
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
        parse_instances_eval(
          """test:seq : sbol:Sequence
            |  sbol:encoding = sbol:IUPACDNA
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    'one_prefix - {
      parse("""@prefix sbol <http://sbols.org/v2#>
              |@defaultPrefix sbol
              |
              |DnaSequence(x) => Sequence
              |  encoding = iupac:DNA
              |  elements = x
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances_eval(
          """sbol:seq : sbol:Sequence
            |  sbol:encoding = iupac:DNA
            |  sbol:elements = "agct"""".stripMargin) in ⊥

    }

    'no_prefix - {
      parse("""DnaSequence(x) => Sequence
              |  encoding = IUPACDNA
              |  elements = x
              |
              |seq : DnaSequence("agct")""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances_eval(
          """seq : Sequence
            |  encoding = IUPACDNA
            |  elements = "agct"""".stripMargin) in ⊥

    }
  }
}
