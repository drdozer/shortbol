package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Mutable
import fastparse.core.Result.Success
import fastparse.parsers.Terminals.{Start, End}
import utest._
import scalaz._
import Scalaz._
import Expander.ops._
import DSL._


/**
 * Created by chris on 17/07/15.
 */


object ExpansionTestSuite extends TestSuite {

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  def parse[T](shortbol: String): Seq[TopLevel] =
    ShortbolParser.SBFile.parse(shortbol) match {
      case s : Success[SBFile] =>
        s.value.tops
    }

  def expanded(libs: Seq[TopLevel], tops: Seq[TopLevel]): Seq[TopLevel] =
    SBFile(libs ++ tops).expansion.eval(ExpansionContext.empty).map(_.tops).flatten

  def expState(libs: Seq[TopLevel], tops: Seq[TopLevel]): ExpansionContext =
    SBFile(libs ++ tops).expansion.exec(ExpansionContext.empty)

  def checkExpansion[T](in: Seq[TopLevel], expected: Seq[TopLevel]): Unit = {
    val expansion = expanded(Seq(), in)
    assert(expansion == expected)
  }

  def checkState[T](in: Seq[TopLevel], expected: ExpansionContext): Unit = {
    val st = expState(Seq(), in)
    assert(st == expected)
  }

  def checkExpansion[T](lib: Seq[TopLevel], in: Seq[TopLevel], expected: Seq[TopLevel]): Unit = {
    val exp = expanded(lib, in)
    assert(exp == expected)
  }

  val tests = TestSuite{

    "nulops" - {

      'blankline - {
        * - checkExpansion(Seq(BlankLine), Seq())
        * - checkState(Seq(BlankLine), ExpansionContext.empty)
      }
      'comment - {
        * - checkExpansion(Seq(Comment("a comment")), Seq())
        * - checkState(Seq(Comment("a comment")), ExpansionContext.empty)
      }
      'template - {
        val foobar = short_c"Foo => Bar"
        * - checkExpansion(Seq(foobar), Seq())
        * - checkState(Seq(foobar), ExpansionContext(Map(foobar.id -> foobar), Map.empty))
      }
    }

    'assignment - {
      * - {
        val aAsB = short_a"a = b"
        * - checkExpansion(Seq(aAsB), Seq(aAsB))
        * - checkState(Seq(aAsB), ExpansionContext(Map.empty, Map(aAsB.property -> aAsB.value)))
      }

      * - {
        val stmts = parse(
          """b = c
            |a = b
          """.stripMargin)

        val resA = short_a"b = c"
        val resB = short_a"a = c"

        * - checkExpansion(stmts, Seq(resA, resB))
        * - checkState(stmts, ExpansionContext(Map.empty, Map(resA.property -> resA.value, resB.property -> resB.value)))
      }
    }

    'individuals - {
      * - checkExpansion(parse("mySeq : Seq"), parse("mySeq : Seq"))

      * - checkExpansion(
        parse("""mySeq : Seq
          |  x = y""".stripMargin),
        parse("""mySeq : Seq
          |  x = y""".stripMargin)
      )

      * - checkExpansion(
        parse("Foo => Bar"),
        parse("foo : Foo"),
        parse("foo : Bar"))

      * - checkExpansion(
        parse(
          """Foo => Bar
            |  x = y
          """.stripMargin),
        parse("foo : Foo"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar()
            |  x = y
          """.stripMargin),
        parse("foo : Foo"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar()
            |  x = y
          """.stripMargin),
        parse("foo : Foo()"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar
            |  x = y
          """.stripMargin),
        parse("foo : Foo()"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo(b) => Bar
            |  a = b""".stripMargin),
        parse("foo : Foo(y)"),
        parse(
          """foo : Bar
            |  a = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar
          """.stripMargin),
          parse(
            """foo : Foo
              |  x = y""".stripMargin),
          parse(
            """foo : Bar
              |  x = y""".stripMargin))

    }

  }

}
