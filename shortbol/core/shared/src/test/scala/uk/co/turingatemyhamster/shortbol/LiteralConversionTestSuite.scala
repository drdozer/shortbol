package uk.co.turingatemyhamster.shortbol

import ast._
import ops._
import utest._

/**
  *
  *
  * @author Matthew Pocock
  */
object LiteralConversionTestSuite extends TestSuite {
  override def tests = TestSuite {
    val fastaString = ShortbolParsers.StringLiteral.parse(
      """{
        |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
        |  aggatcggcg gttttctttt ctcttctcaa
        |  }^^edam:fasta""".stripMargin).get.value

    val genbankString = ShortbolParsers.StringLiteral.parse(
      """{
        |        1 ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
        |       61 aggatcggcg gttttctttt ctcttctcaa
        |}^^edam:genbank""".stripMargin).get.value

    val expected = ShortbolParsers.StringLiteral.parse(
      "\"ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa\"").get.value

    def matchingStrings(observed: Option[Literal], expected: StringLiteral) =
      assert(observed exists (_.asInstanceOf[StringLiteral].style.asString == expected.style.asString))

    'fasta - {
      'forFasta - {
        val c = DNAFormatConversion.fastaToDNA(
          fastaString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }

      'forGenbank - {
        val c = DNAFormatConversion.fastaToDNA(
          genbankString,
          DNAFormatConversion.XsdString)

        assert(c.isEmpty)
      }

      'forFastaGenbank - {
        val c = LiteralConversion(DNAFormatConversion.fastaToDNA, DNAFormatConversion.genbankToDNA)(
          fastaString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }

      'forGenbankFasta - {
        val c = LiteralConversion(DNAFormatConversion.genbankToDNA, DNAFormatConversion.fastaToDNA)(
          fastaString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }
    }

    'genbank - {
      'forFasta - {
        val c = DNAFormatConversion.genbankToDNA(
          fastaString,
          DNAFormatConversion.XsdString)

        assert(c.isEmpty)
      }

      'forGenbank - {
        val c = DNAFormatConversion.genbankToDNA(
          genbankString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }

      'forFastaGenbank - {
        val c = LiteralConversion(DNAFormatConversion.fastaToDNA, DNAFormatConversion.genbankToDNA)(
          genbankString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }

      'forGenbankFasta - {
        val c = LiteralConversion(DNAFormatConversion.genbankToDNA, DNAFormatConversion.fastaToDNA)(
          genbankString,
          DNAFormatConversion.XsdString)

        matchingStrings(c, expected)
      }
    }

    'typeError- {
      val ontology =
        """
          |Sequence : owl:Class
          |  elements : owl:propertyRestriction
          |    owl:allValuesFrom = xsd:string
        """.stripMargin
      import ast.sugar._
      import Eval.EvalOps
      import ShortbolParser.POps
      val ontologyCtxt = ShortbolParser.SBFile.withPositions("_ontology_", ontology).get.value.eval.exec(Fixture.configuredContext)

      val owl = OWL.fromContext(ontologyCtxt)

      val litConv = LiteralConversion(DNAFormatConversion.fastaToDNA, DNAFormatConversion.genbankToDNA)

      'typeStringLiteral - {
        val expRec = Assignment("elements", expected) : BodyStmt
        val cstr = owl.allValuesFromConstraint("elements", Assignment("owl" :# "allValuesFrom", "xsd" :# "string"))
        val tpeCheck = cstr.get.apply(Assignment("elements", fastaString) : BodyStmt)
        val recovered = tpeCheck.leftMap(_.map(_.recoverWith(litConv)))

        recovered.fold(
          nel => assert(nel.head.contains(None -> expRec)),
          a => assert(a != a))
      }
    }
  }
}
