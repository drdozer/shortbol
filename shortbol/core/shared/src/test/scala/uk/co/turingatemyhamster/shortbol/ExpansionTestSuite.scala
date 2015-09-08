package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Mutable
import fastparse.core.Result.Success
import fastparse.parsers.Terminals.{Start, End}
import utest._
import scalaz._
import Scalaz._
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

  def expanded(tops: Seq[TopLevel]): Seq[TopLevel] =
    Fixture.expand(SBFile(tops))._2.map(_.tops).flatten

  def expanded(tops: Seq[TopLevel], ctxt: ExpansionContext): Seq[TopLevel] =
    Fixture.expand(SBFile(tops), ctxt)._2.map(_.tops).flatten

  def expState(libs: Seq[TopLevel], tops: Seq[TopLevel]): ExpansionContext =
    Fixture.expand(SBFile(libs ++ tops))._1

  def checkExpansion[T](in: Seq[TopLevel], expected: Seq[TopLevel]): Unit = {
    val expansion = expanded(in)
    assert(expansion == expected)
  }

  val pp = Fixture.prettyPrinter(System.out)

  def checkExpansion[T](imports: (Identifier, Seq[TopLevel])*)
                       (in: Seq[TopLevel], expected: Seq[TopLevel]): Unit =
  {
    val ctxt = Fixture.emptyContext.copy(
      rslvr = Resolver.fromValues(imports map { case (i, ts) => i -> SBFile(ts) } :_*))
    val expansion = expanded(in, ctxt)
    println("%% expansion")
    pp.append(expansion)
    println()
    println("%% expected")
    pp.append(expected)
    println()
    assert(expansion == expected)
  }

  def checkState[T](in: Seq[TopLevel], expected: ExpansionContext): Unit = {
    val st = expState(Seq(), in)
    assert(st == expected)
  }

  val tests = TestSuite{

    "nullops" - {

      'blankline - {
        * - checkExpansion(Seq(BlankLine), Seq())
        * - checkState(Seq(BlankLine), Fixture.emptyContext)
      }
      'comment - {
        * - checkExpansion(Seq(Comment("a comment")), Seq())
        * - checkState(Seq(Comment("a comment")), Fixture.emptyContext)
      }
      'template - {
        val foobar = short_c"Foo => Bar"
        * - checkExpansion(Seq(foobar), Seq())
        * - checkState(Seq(foobar), Fixture.emptyContext.copy(cstrs = Map(foobar.id -> foobar)))
      }
    }

    'assignment - {
      * - {
        val aAsB = short_a"a = b"
        * - checkExpansion(Seq(aAsB), Seq(aAsB))
        * - checkState(Seq(aAsB), Fixture.emptyContext.copy(bndgs = Map(aAsB.property -> aAsB.value)))
      }

      * - {
        val stmts = parse(
          """b = c
            |a = b
          """.stripMargin)

        val resA = short_a"b = c"
        val resB = short_a"a = c"

        * - checkExpansion(stmts, Seq(resA, resB))
        * - checkState(stmts, Fixture.emptyContext.copy(bndgs = Map(resA.property -> resA.value, resB.property -> resB.value)))
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
        parse("Foo => Bar") ++
        parse("foo : Foo"),
        parse("foo : Bar"))

      * - checkExpansion(
        parse(
          """Foo => Bar
            |  x = y
          """.stripMargin) ++
        parse("foo : Foo"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar()
            |  x = y
          """.stripMargin) ++
        parse("foo : Foo"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar()
            |  x = y
          """.stripMargin) ++
        parse("foo : Foo()"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar
            |  x = y
          """.stripMargin) ++
        parse("foo : Foo()"),
        parse(
          """foo : Bar
            |  x = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo(b) => Bar
            |  a = b""".stripMargin) ++
        parse("foo : Foo(y)"),
        parse(
          """foo : Bar
            |  a = y""".stripMargin))

      * - checkExpansion(
        parse(
          """Foo => Bar
          """.stripMargin) ++
          parse(
            """foo : Foo
              |  x = y""".stripMargin),
          parse(
            """foo : Bar
              |  x = y""".stripMargin))

    }

    'imports - {

      * - checkExpansion()(
        parse("""mySeq : Seq
                |  x = y""".stripMargin),
        parse("""mySeq : Seq
                |  x = y""".stripMargin)
      )

      * - checkExpansion(Url("exampleImport") -> Seq())(
        parse("""mySeq : Seq
                |  x = y""".stripMargin),
        parse("""mySeq : Seq
                |  x = y""".stripMargin)
      )

      * - checkExpansion(Url("exampleImport") -> parse("""Foo => Bar"""))(
        parse("""mySeq : Seq
                |  x = y""".stripMargin),
        parse("""mySeq : Seq
                |  x = y""".stripMargin)
      )

      * - checkExpansion(Url("exampleImport") -> parse("""Foo => Bar"""))(
        parse("""import <exampleImport>
                |mySeq : Seq
                |  x = y""".stripMargin),
        ProcessedImport(Url("exampleImport"), SBFile(Seq())) +:
          parse("""mySeq : Seq
                |  x = y""".stripMargin)
      )

      * - checkExpansion(Url("exampleImport") -> parse("""cat : Animal"""))(
        parse("""import <exampleImport>
                |mySeq : Seq
                |  x = y""".stripMargin),
        ProcessedImport(Url("exampleImport"), SBFile(parse("""cat : Animal"""))) +:
          parse("""mySeq : Seq
                  |  x = y""".stripMargin)
      )

      * - checkExpansion(Url("exampleImport") -> parse("""Foo => Bar"""))(
        parse("""import <exampleImport>
                |mySeq : Foo
                |  x = y""".stripMargin),
        ProcessedImport(Url("exampleImport"), SBFile(Seq())) +:
          parse("""mySeq : Bar
                  |  x = y""".stripMargin)
      )

    }
  }

}
