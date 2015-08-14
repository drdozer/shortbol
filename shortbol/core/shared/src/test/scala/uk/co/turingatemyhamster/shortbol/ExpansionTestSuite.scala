package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Mutable
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


  val parser = new ShortbolParser()

  def parse[T](shortbol: String, p: Parser[T]): Seq[TopLevel] =
    (Start ~ p ~ End).parse(shortbol) match {
      case s : Mutable.Success[Seq[TopLevel]] =>
        s.value
    }

  def expanded(tops: Seq[TopLevel]): Seq[TopLevel] = {
    val cstrs = Ops.constructors(tops)
    val inds = Ops.individuals(tops)
    val ex = ExpansionContext(cstrs, Bindings(Map()))

    inds.byId.values.toSeq flatMap (_ expandWith ex)
  }

  def checkExpansion[T](in: String,out: String,p: Parser[T]): Unit = {
    var text = in + "\n"
//    println(escape(text))
//    println(escape(out))
    val parsedin = parse(text,p)
    val parsedout = parse(out,p)

    val expandedin = expanded(parsedin)
    println("------------------------------------------------------------------------------------------------")
    println("----------------------------------------------------------------------")
    println(parsedin)
    println("----------------------------------------------------------------------")
    println(parsedout)
    println("----------------------------------------------------------------------")
    println(expandedin)
    println("----------------------------------------------------------------------")
    println("------------------------------------------------------------------------------------------------")

    assert(expandedin == parsedout)

  }

  val tests = TestSuite{

    "Expansion" - {

      val input =
        """DNASequence(x) => Sequence
        |   elements = x
        |   encoding = <SBOL:DNA>
        |
        |
        |cds_sequence : DNASequence("AAATG")""".stripMargin

      val output =  """cds_sequence : Sequence
          |   elements = "AAATG"
          |   encoding = <SBOL:DNA>
          |""".stripMargin

      val raw =
        """T(a) => F
          |  name = a
          |
          |ta : T("bob")
          |""".stripMargin

      checkExpansion(raw,output,parser.TopLevels)

//      checkExpansion(
//        """
//          |seq => Sequence
//          |  a = b
//          |  a = b
//          |
//          |BBa_J611210_seq : seq""".stripMargin,"blank",parser.TopLevels
//      )

    }


  }


}
