package uk.co.turingatemyhamster.shortbol

import shorthandAst._
import shorthandAst.sugar._
import ops._
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.{RepairSequence}
import utest._
import RewriteRule.{FilteringOps, Rewritten}

import scalaz.{-\/, Scalaz, \/, \/-}
import Scalaz._

/**
  *
  *
  * @author Matthew Pocock
  */
object RewriteRuleTestSuite extends TestSuite {
  val Ø = Fixture.emptyContext

  def rewrittenToPropertyExp(observed: RewriteRule.MaybeRewritten[longhandAst.PropertyExp], expectedProperty: Identifier, expectedValue: StringLiteral) = new {
    def producing(expInstances: List[longhandAst.InstanceExp]) = new {
      def in (c: EvalContext) = {
        observed.eval(c) match {
          case \/-(pvExp) =>
            val (newInstances, v) = pvExp.run
            v match {
              case longhandAst.PropertyExp(pid, v) =>
                assert(pid == expectedProperty, expInstances == newInstances)
                rewrittenToPropertyValue(v.point[Rewritten].right[longhandAst.PropertyValue].point[EvalState], expectedValue) in c
            }
        }
      }
    }

    def in (c: EvalContext) = producing(Nil) in c
  }

  def rewrittenToPropertyValue(observed: RewriteRule.MaybeRewritten[longhandAst.PropertyValue], expected: StringLiteral) = new {
    def in (c: EvalContext) =
      observed.eval(c) match {
        case \/-(r) =>
          val (newInds, v) = r.run
          rewrittenToPropertyValueLiteral(v.asInstanceOf[longhandAst.PropertyValue.Literal].point[Rewritten].right[longhandAst.PropertyValue.Literal].point[EvalState], expected) in c
      }
  }

  def rewrittenToPropertyValueLiteral(observed: RewriteRule.MaybeRewritten[longhandAst.PropertyValue.Literal], expected: StringLiteral) = new {
    def in (c: EvalContext) = observed.eval(c) match {
      case \/-(r) =>
        val (newInds, v) = r.run
        rewrittenToLiteral(v.value.point[Rewritten].right[Literal].point[EvalState], expected) in c
    }
  }

  def rewrittenToLiteral(observed: RewriteRule.MaybeRewritten[Literal], expected: StringLiteral) = new {
    def in (c: EvalContext) =
      observed.eval(c) match {
        case \/-(r) =>
          val (newInds, v) = r.run
          v match {
            case StringLiteral(s, _, _) =>
              assert(s.asString == expected.style.asString)
          }
      }
  }


  def notRewritten[T](observed: RewriteRule.MaybeRewritten[T]) = new {
    def in (c: EvalContext) = observed.eval(c) match {
      case -\/(_) =>
      case \/-(_) =>
        assert(false)
    }
  }

  def notRewritten[T](observed: RewriteRule.MaybeRewritten[T], expected: T) = new {
    def in (c: EvalContext) = observed.eval(c) match {
      case -\/(value) =>
        assert(value == expected)
      case \/-(_) =>
        assert(false)
    }
  }


  def rewrittenTo[T](r: RewriteRule.MaybeRewritten[T], expected: T, expectedExtras: List[longhandAst.InstanceExp] = Nil) = new {
    def in (c: EvalContext) = r.eval(c) match {
      case \/-(rr) =>
        val (extras, value) = rr.run
        assert(value == expected, extras == expectedExtras)
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

      val fromFastaOrGenbank = RepairSequence.repairToDNA

      'atLiteral - {
        'fasta - {
          'forFasta - {
            val c = RepairSequence.fastaToDNA(fastaString)
            rewrittenToLiteral(c, expected) in Ø
          }

          'forGenbank - {
            val c = RepairSequence.fastaToDNA(genbankString)
            notRewritten(c) in Ø
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
            val c = RepairSequence.genbankToDNA(fastaString)
            notRewritten(c) in Ø
          }

          'forGenbank - {
            val c = RepairSequence.genbankToDNA(genbankString)
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
        val fogAtPropertyExp = fogAtPE at
          (optics.longhand.PropertyExp.property :== "elements")
        val fogAtElements = fogAtPropertyExp at RewriteRule.allElements

        'propertyValue_Literal - {
          val c = fogAtPVL(longhandAst.PropertyValue.Literal(fastaString))
          rewrittenToPropertyValueLiteral(c, expected) in Ø
        }

        'propertyValue - {
          val c = fogAtPV(longhandAst.PropertyValue.Literal(fastaString) : longhandAst.PropertyValue)
          rewrittenToPropertyValue(c, expected) in Ø
        }

        'propertyExp - {
          val c = fogAtPE(longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString)))
          rewrittenToPropertyExp(c, "elements", expected) in Ø
        }

        'propertyExpFilterFromSequence - {
          val c = fogAtPropertyExp(longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString)))
          rewrittenToPropertyExp(c, "elements", expected) in Ø
        }

        'propertyExpFilterFromOther - {
          val c = fogAtPropertyExp(longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)))
          notRewritten(c) in Ø
        }

        'propertyExpFilterAll - {
          'ss - {
            val c = fogAtElements(List(
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(expected)),
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(expected)))) in Ø
          }
          'so - {
            val c = fogAtElements(List(
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(expected)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)))) in Ø
          }
          'os - {
            val c = fogAtElements(List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(fastaString))
            ))
            rewrittenTo(c, List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("elements", longhandAst.PropertyValue.Literal(expected)))) in Ø
          }
          'oo - {
            val c = fogAtElements(List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString))
            ))
            notRewritten(c, List(
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)),
              longhandAst.PropertyExp("other", longhandAst.PropertyValue.Literal(fastaString)))) in Ø
          }
        }
      }
    }

  }
}
