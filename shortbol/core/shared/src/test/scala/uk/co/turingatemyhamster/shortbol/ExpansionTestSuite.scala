//package uk.co.turingatemyhamster.shortbol
//
//import fastparse.core.Result
//import utest._
//import Expander.ops._
///**
// * Created by chris on 17/07/15.
// */
//object ExpansionTestSuite extends TestSuite {
//  val parser = new ShortbolParser()
//  def parse(shortbol: String): Seq[TopLevel] =
//    parser.TopLevels.parse(shortbol) match {
//      case s : Result.Success.Mutable[Seq[TopLevel]] =>
//        s.value
//    }
//
//  def expanded(tops: Seq[TopLevel]): Seq[TopLevel] = {
//    val cstrs = Ops.constructors(tops)
//    val inds = Ops.individuals(tops)
//    val ex = ExpansionContext(cstrs, Bindings(Map()))
//
//    inds.byId.values.toSeq flatMap (_ expandWith ex)
//  }
//
//  def checkExpansion(in: String,out: String): Unit = {
//    val parsedin = parse(in)
//    val parsedout = parse(out)
//
//    val expandedin = expanded(parsedin)
//
//    assert(expandedin == parsedout)
//
//  }
//
//  val tests = TestSuite{
//
//    "always passes" - assert(true)
//
//
//    "hopefully passes" - {
//
//      val input = """
//        |DNASequence(x) => Sequence
//        | elements = x
//        | encoding = <SBOL:DNA>
//        |
//        |
//        |cds_sequence : DNASequence("AAATG")
//        |
//      """.stripMargin
//
//      val output =  """
//          |cds_sequence : Sequence
//          | elements = "AAATG"
//          | encoding = <SBOL:DNA>
//        """.stripMargin
//
//      checkExpansion(input,output)
//
//
//    }
//
//
//  }
//
//
//}
