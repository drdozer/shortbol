//package uk.co.turingatemyhamster.shortbol
//
//import fastparse._
//import utest._
////import all._
///**
// * Same as MathTests, but demonstrating the use of whitespace
// */
//
//
//
//
//object IndentationTests extends TestSuite{
//  def eval(tree: (String, Seq[Int])) = tree match{
//    case ("+", nums) => nums.reduceLeft(_+_)
//    case ("-", nums) => nums.reduceLeft(_-_)
//    case ("*", nums) => nums.reduceLeft(_*_)
//    case ("/", nums) => nums.reduceLeft(_/_)
//  }
//
//  /**
//   * Parser for an indentation-based math syntax. Parens are no longer
//   * necessary, and the whole parser is parametrized with the current
//   * depth of indentation
//   */
//  class Parser(indent: Int,previousIndent: Int){
//
//    var chosenIndent = indent - previousIndent
//
//    val number: P[Int] = P( CharIn('0'to'9').rep(1).!.map(_.toInt) )
//
//    var howdeep = P( (" ".rep(indent + 1)).!.map(_.length))
//
//    if(indent != 0){
//      howdeep = P((" " * (indent + chosenIndent)).!.map(_.length))
//    }
//
//    val deeper = howdeep
//    val blockBody: P[Seq[Int]] = Nl.rep(1) ~ deeper.flatMap(i =>
//      new Parser(indent = i,indent).factor.rep(1, Nl.rep(1) ~ (" " * i) ~!) log "br"
//    )
//
//    def Nl = P("\r\n" | "\r" | "\n")
//
////    def Indent = P(Nl.rep(1) ~ " " * i)
//
//    val block: P[Int] = P( CharIn("+-*/").! ~! blockBody).map(eval)
//
//    val factor: P[Int] = P( number | block )
//
//    val expr: P[Int]   = P( block ~ End )
//  }
//  val expr = new Parser(indent = 0,previousIndent = 0).expr
//  val tests = TestSuite {
//    'IndentationTest {
//      def check(str: String, num: Int) = {
//        val Result.Success(value, _) = expr.parse(str)
//        assert(value == num)
//      }
//
//      check(
//        """+
//          |
//          |
//          | 1
//          | 1
//        """.stripMargin.trim,
//        2
//      )
//            check(
//              """+
//                |   1
//                |   *
//                |      1
//                |
//                |      2
//              """.stripMargin.trim,
//              3
//            )
//
//            check(
//              """+
//                |   +
//                |      1
//                |      *
//                |         1
//                |         2
//                |      2
//              """.stripMargin.trim,
//              5
//            )
//
//            check(
//              """+
//                |  +
//                |    1
//                |    *
//                |      1
//                |      2
//                |  *
//                |    3
//                |    4
//                |    5
//                |
//              """.stripMargin.trim,
//              63
//            )
//            check(
//              """/
//                |  15
//                |  3
//              """.stripMargin.trim,
//              5
//            )
//            check(
//              """/
//                |  63
//                |  3
//              """.stripMargin.trim,
//              21
//            )
//            check(
//              """+
//                |  +
//                |    1
//                |    *
//                |      1
//                |      2
//                |  /
//                |    *
//                |      3
//                |      4
//                |      5
//                |    20
//              """.stripMargin.trim,
//              6
//            )
//            check(
//              """/
//                |  +
//                |    +
//                |      1
//                |      *
//                |        1
//                |        2
//                |      *
//                |        3
//                |        4
//                |        5
//                |  3
//              """.stripMargin.trim,
//              21
//            )
//          }
//      //    'fail{
//      //      def check(input: String, expectedTrace: String) = {
//      //        val failure = expr.parse(input).asInstanceOf[Result.Failure]
//      //        val actualTrace = failure.traced.trace
//      //        assert(expectedTrace.trim == actualTrace.trim)
//      //      }
//      //      * - check(
//      //        "+",
//      //        """ expr:0 / block:0 / "\n":1 ..."" """
//      //      )
//      //      * - check(
//      //        """+
//      //          |  1
//      //          |1
//      //        """.stripMargin.trim,
//      //        """ expr:0 / (End | "\n  "):5 ..."\n1" """
//      //      )
//      //      * - check(
//      //        """+
//      //          |  1
//      //          |   1
//      //        """.stripMargin.trim,
//      //        """ expr:0 / block:0 / factor:8 / (number | block):8 ..." 1" """
//      //      )
//      //    }
//    }
//
//
//}