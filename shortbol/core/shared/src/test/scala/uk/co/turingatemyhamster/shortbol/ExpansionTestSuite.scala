package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Mutable
import fastparse.core.Result.Success
import fastparse.parsers.Terminals.{Start, End}
import utest._
import Expander.ops._


/**
 * Created by chris on 17/07/15.
 */


object ExpansionTestSuite extends TestSuite {

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  def parse[T](shortbol: String): Seq[TopLevel] =
    ShortbolParser.File.parse(shortbol) match {
      case s : Success[Seq[TopLevel]] =>
        s.value
    }

  def expanded(tops: Seq[TopLevel]): Seq[TopLevel] = {
    val cstrs = Ops.constructors(tops)
    val inds = Ops.individuals(tops)
    val ex = ExpansionContext(cstrs, Bindings(Map()))

    inds.byId.values.toSeq flatMap (_ expandWith ex)
  }

  def checkExpansion[T](in: Seq[TopLevel], expected: Seq[TopLevel]): Unit = {

    val expandedin = expanded(in)

    assert(expandedin == expected)
  }

  val tests = TestSuite{

    "Expansion" - {

      'blankline - checkExpansion(parse("\n"), Seq())
      'comment - checkExpansion(parse("# a comment"), Seq())
      'template - checkExpansion(parse("Foo => Bar"), Seq())
      'assignment - checkExpansion(parse("a = b"), Seq())

      'individuals - {
        'anIndividual - checkExpansion(parse("mySeq : Seq"), Seq())
      }
//      val input =
//        parse("""DNASequence(x) => Sequence
//        |   elements = x
//        |   encoding = <SBOL:DNA>
//        |
//        |
//        |cds_sequence : DNASequence("AAATG")""".stripMargin)
//
//      val output =  parse("""cds_sequence : Sequence
//          |   elements = "AAATG"
//          |   encoding = <SBOL:DNA>
//          |""".stripMargin)
//
//      val raw =
//        parse("""T(a) => F
//          |  name = a
//          |
//          |ta : T("bob")
//          |""".stripMargin)
//
//      checkExpansion(raw,output)
//
//      checkExpansion(
//        parse("""
//          |seq => Sequence
//          |  a = b
//          |  a = b
//          |
//          |BBa_J611210_seq : seq""".stripMargin), Seq()
//      )
//
    }


  }


}
