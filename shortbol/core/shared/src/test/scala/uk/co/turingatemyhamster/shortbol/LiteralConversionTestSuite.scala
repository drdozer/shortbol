package uk.co.turingatemyhamster.shortbol

import shorthandAst._
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

    def rewrittenTo(observed: RewriteRule.Rewritten[Literal], expected: StringLiteral) = new {
      def in (c: EvalContext) =
        for {
          o <- observed
        } {
          o.eval(c) match {
            case StringLiteral(s, _, _) =>
              assert(s.asString == expected.style.asString)
          }
        }
    }

    def notRewritten(observed: RewriteRule.Rewritten[Literal]) =
      assert(observed.isLeft)

    'fasta - {
      'forFasta - {
        val c = DNAFormatConversion.fastaToDNA(fastaString)
        rewrittenTo(c, expected)
      }

      'forGenbank - {
        val c = DNAFormatConversion.fastaToDNA(genbankString)
        notRewritten(c)
      }

      'forFastaGenbank - {
        val c = (DNAFormatConversion.fastaToDNA or DNAFormatConversion.genbankToDNA)(
          fastaString)
        rewrittenTo(c, expected)
      }

      'forGenbankFasta - {
        val c = (DNAFormatConversion.genbankToDNA or DNAFormatConversion.fastaToDNA)(
          fastaString)
        rewrittenTo(c, expected)
      }
    }

    'genbank - {
      'forFasta - {
        val c = DNAFormatConversion.genbankToDNA(fastaString)
        notRewritten(c)
      }

      'forGenbank - {
        val c = DNAFormatConversion.genbankToDNA(genbankString)
        rewrittenTo(c, expected)
      }

      'forFastaGenbank - {
        val c = (DNAFormatConversion.fastaToDNA or DNAFormatConversion.genbankToDNA)(
          genbankString)
        rewrittenTo(c, expected)
      }

      'forGenbankFasta - {
        val c = (DNAFormatConversion.genbankToDNA or DNAFormatConversion.fastaToDNA)(
          genbankString)
        rewrittenTo(c, expected)
      }
    }

//    'typeError- {
//      val ontology =
//        """
//          |Sequence : owl:Class
//          |  elements : owl:propertyRestriction
//          |    owl:allValuesFrom = xsd:string
//        """.stripMargin
//      import ast.sugar._
//      import Eval.EvalOps
//      import ShortbolParser.POps
//      val ontologyCtxt = ShortbolParser.SBFile.withPositions("_ontology_", ontology).get.value.eval.exec(Fixture.configuredContext)
//
//      val owl = OWL.fromContext(ontologyCtxt)
//
//      val litConv = LiteralConversion(DNAFormatConversion.fastaToDNA, DNAFormatConversion.genbankToDNA)
//
//      'typeStringLiteral - {
//        val expRec = Assignment("elements", expected) : BodyStmt
//        val cstr = owl.allValuesFromConstraint("elements", Assignment("owl" :# "allValuesFrom", "xsd" :# "string"))
//        val tpeCheck = cstr.get.apply(Assignment("elements", fastaString) : BodyStmt)
//        val recovered = tpeCheck.leftMap(_.map(_.recoverWith(litConv)))
//
//        recovered.fold(
//          nel => assert(nel.head.contains(None -> expRec)),
//          a => assert(a != a))
//      }
//    }
  }
}
