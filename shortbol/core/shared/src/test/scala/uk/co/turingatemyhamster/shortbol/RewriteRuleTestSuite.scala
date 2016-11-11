package uk.co.turingatemyhamster.shortbol

import shorthandAst._
import shorthandAst.sugar._
import ops._
import uk.co.turingatemyhamster.shortbol.longhandAst.PropertyExp
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.DNAFormatRewriteRule
import utest._

import scalaz.{-\/, \/, \/-}

/**
  *
  *
  * @author Matthew Pocock
  */
object RewriteRuleTestSuite extends TestSuite {
  val Ø = Fixture.emptyContext

  def rewrittenToPropertyExp(observed: RewriteRule.Rewritten[longhandAst.PropertyExp], expectedProperty: Identifier, expectedValue: StringLiteral) = new {
    def in (c: EvalContext) = {
      observed match {
        case \/-(pvExp) =>
          pvExp eval c match {
            case longhandAst.PropertyExp(pid, _) =>
              assert(pid == expectedProperty)
          }
      }
      rewrittenToPropertyValue(observed.bimap(_ => ???, _ map (_.value)), expectedValue) in c
    }
  }

  def rewrittenToPropertyValue(observed: RewriteRule.Rewritten[longhandAst.PropertyValue], expected: StringLiteral) =
    rewrittenToPropertyValueLiteral(observed.bimap(_ => ???, _ map (_.asInstanceOf[longhandAst.PropertyValue.Literal])), expected)

  def rewrittenToPropertyValueLiteral(observed: RewriteRule.Rewritten[longhandAst.PropertyValue.Literal], expected: StringLiteral) =
    rewrittenToLiteral(observed.bimap(_.value, _ map (_.value)), expected)

  def rewrittenToLiteral(observed: RewriteRule.Rewritten[Literal], expected: StringLiteral) = new {
    def in (c: EvalContext) =
      observed.foreach(_ eval c match {
        case StringLiteral(s, _, _) =>
          assert(s.asString == expected.style.asString)
      })
  }


  def notRewritten[T](observed: RewriteRule.Rewritten[T]) =
    assert(observed.isLeft)

  def notRewritten[T](observed: RewriteRule.Rewritten[T], expected: T) = {
    assert(observed.isLeft)
    observed match {
      case -\/(value) =>
        assert(value == expected)
    }
  }


  def rewrittenTo[T](r: RewriteRule.Rewritten[T], expected: T) = new {
    def in (c: EvalContext) = {
      assert(r.isRight)
      r match {
        case \/-(rr) =>
          val value = rr.eval(c)
          assert(value == expected)
      }
    }
  }

  override def tests = TestSuite {
    'dnaFormatConversions - {

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

      val fromFastaOrGenbank = DNAFormatRewriteRule.fastaToDNA or DNAFormatRewriteRule.genbankToDNA

      'atLiteral - {
        'fasta - {
          'forFasta - {
            val c = DNAFormatRewriteRule.fastaToDNA(fastaString)
            rewrittenToLiteral(c, expected) in Ø
          }

          'forGenbank - {
            val c = DNAFormatRewriteRule.fastaToDNA(genbankString)
            notRewritten(c)
          }

          'forFastaGenbank - {
            val c = fromFastaOrGenbank(fastaString)
            rewrittenToLiteral(c, expected) in Ø
          }

          'forGenbankFasta - {
            val c = fromFastaOrGenbank(fastaString)
            rewrittenToLiteral(c, expected) in Ø
          }
        }

        'genbank - {
          'forFasta - {
            val c = DNAFormatRewriteRule.genbankToDNA(fastaString)
            notRewritten(c)
          }

          'forGenbank - {
            val c = DNAFormatRewriteRule.genbankToDNA(genbankString)
            rewrittenToLiteral(c, expected) in Ø
          }

          'forFastaGenbank - {
            val c = fromFastaOrGenbank(genbankString)
            rewrittenToLiteral(c, expected) in Ø
          }

          'forGenbankFasta - {
            val c = fromFastaOrGenbank(genbankString)
            rewrittenToLiteral(c, expected) in Ø
          }
        }
      }

      'atPropertyValue - {

        val fogAtPVL = fromFastaOrGenbank at
          optics.longhand.PropertyValue.Literal.value
        val fogAtPV = fromFastaOrGenbank at
          optics.longhand.PropertyValue.Literal.value at
          optics.longhand.PropertyValue.asLiteral
        val fogAtPE = fromFastaOrGenbank at
          optics.longhand.PropertyValue.Literal.value at
          optics.longhand.PropertyValue.asLiteral at
          optics.longhand.PropertyExp.value
        val fogAtSequence = fogAtPE at ((_: longhandAst.PropertyExp).property == ("sequence": Identifier))
        val fogAtSequences = fogAtSequence at RewriteAt.allElements

        'propertyValue_Literal - {
          val c = fogAtPVL(longhandAst.PropertyValue.Literal(fastaString))
          rewrittenToPropertyValueLiteral(c, expected) in Ø
        }

        'propertyValue - {
          val c = fogAtPV(longhandAst.PropertyValue.Literal(fastaString) : longhandAst.PropertyValue)
          rewrittenToPropertyValue(c, expected) in Ø
        }

        'propertyExp - {
          val c = fogAtPE(longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString)))
          rewrittenToPropertyExp(c, "sequence", expected) in Ø
        }

        'propertyExpFilterFromSequence - {
          val c = fogAtSequence(longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString)))
          rewrittenToPropertyExp(c, "sequence", expected) in Ø
        }

        'propertyExpFilterFromOther - {
          val c = fogAtSequence(longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)))
          notRewritten(c)
        }

        'propertyExpFilterAll - {
          'ss - {
            val c = fogAtSequences(List(
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(expected)),
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(expected)))) in Ø
          }
          'so - {
            val c = fogAtSequences(List(
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(expected)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)))) in Ø
          }
          'os - {
            val c = fogAtSequences(List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("sequence", longhandAst.PropertyValue.Literal(expected)))) in Ø
          }
          'oo - {
            val c = fogAtSequences(List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString))
            ))
            notRewritten(c, List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString))))
          }
        }
      }
    }

  }
}
