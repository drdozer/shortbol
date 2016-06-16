package uk.co.turingatemyhamster.shortbol

import fastparse.core.Parsed.Success
import fastparse.parsers.Terminals.{End, Start}
import utest._

import scalaz._
import Scalaz._
import uk.co.turingatemyhamster.shortbol.ast._
import uk.co.turingatemyhamster.shortbol.ops._
import Eval.EvalOps
import ast.sugar._


/**
 * Created by chris on 17/07/15.
 */


object EvalTestSuite extends TestSuite {
//
//  def escape(raw: String): String = {
//    import scala.reflect.runtime.universe._
//    Literal(Constant(raw)).toString
//  }
//
  def parse[T](shortbol: String): Seq[TopLevel] =
    ShortbolParser.SBFile.parse(shortbol) match {
      case s : Success[SBFile] =>
        s.value.tops
    }

  import Fixture.emptyContext

  implicit class TestOps[T](_t: T) {

    def inContext(c0: EvalContext) = new InContext(_t, c0)
    def inContext(as: Assignment*) = new InContext(_t, emptyContext.copy(bndgs = Map.empty ++ as.map(a => a.property -> a.value)))
    def evaluatesTo[U](u: U)(implicit e: EvalEval.Aux[T, U]) = (new InContext(_t, emptyContext)).evaluatesTo(u)(e)
  }

  class InContext[T](t: T, c0: EvalContext) {
    def evaluatesTo[U](u: U)(implicit e: EvalEval.Aux[T, U]) = new Object {
      def withContext(as: Assignment*): Unit = withContext(emptyContext.copy(bndgs = Map.empty ++ as.map(a => a.property -> a.value)))
      def withContext(c: EvalContext): Unit = {
        val (cc, uu) = e(t).run(c0)
        assert(cc == c)
        assert(uu == u)
      }
    }
  }

  val tests = TestSuite {

    'blankline - {
      * - { BlankLine evaluatesTo BlankLine withContext emptyContext }
      * - { (BlankLine : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) withContext emptyContext }
      * - { (BlankLine : BodyStmt) evaluatesTo (BlankLine : BodyStmt) withContext emptyContext }
    }

    'comment - {
      * - { Comment("a comment") evaluatesTo Comment("a comment") withContext emptyContext }
      * - { (Comment("a comment") : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) withContext emptyContext }
      * - { (Comment("a comment") : BodyStmt) evaluatesTo (Comment("a comment") : BodyStmt) withContext emptyContext }
    }

    'stringLiteral - {
      * - { StringLiteral("abc", false) evaluatesTo StringLiteral("abc", false) withContext emptyContext }
    }

    'multiLineLiteral - {
      * - { MultiLineLiteral(Seq("abc", "def"), 4) evaluatesTo MultiLineLiteral(Seq("abc", "def"), 4) withContext emptyContext }
    }

    'integerLiteral - {
      * - { IntegerLiteral(42) evaluatesTo IntegerLiteral(42) withContext emptyContext }
    }

    'literal - {
      * - { (StringLiteral("abc", false) : Literal) evaluatesTo (StringLiteral("abc", false) : Literal) withContext emptyContext }
      * - { (MultiLineLiteral(Seq("abc", "def"), 4) : Literal) evaluatesTo (MultiLineLiteral(Seq("abc", "def"), 4) : Literal) withContext emptyContext }
      * - { (IntegerLiteral(42) : Literal) evaluatesTo (IntegerLiteral(42) : Literal) withContext emptyContext }
    }

    'localName - {
      * - { LocalName("a") evaluatesTo ("a" : Identifier) withContext emptyContext }
      * - { LocalName("a") inContext ("a" -> "x") evaluatesTo ("x" : Identifier) withContext ("a" -> "x") }
      * - { LocalName("a") inContext ("a" -> Url("x")) evaluatesTo (Url("x") : Identifier) withContext ("a" -> Url("x")) }
      * - { LocalName("a") inContext ("a" -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) withContext ("a" -> QName("foo", "bar")) }
      * - { LocalName("a") inContext (QName("foo", "a") -> "x") evaluatesTo ("x" : Identifier) withContext (QName("foo", "a") -> "x") }
      * - { LocalName("a") inContext ("a" -> 42) evaluatesTo ("a" : Identifier) withContext ("a" -> 42) }
    }

    'url - {
      * - { Url("a") evaluatesTo (Url("a") : Identifier) withContext emptyContext }
      * - { Url("a") inContext (Url("a") -> "x") evaluatesTo ("x" : Identifier) withContext (Url("a") -> "x") }
      * - { Url("a") inContext (Url("a") -> Url("x")) evaluatesTo (Url("x") : Identifier) withContext (Url("a") -> Url("x")) }
      * - { Url("a") inContext (Url("a") -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) withContext (Url("a") -> QName("foo", "bar")) }
    }

    'qname - {
      * - { QName("pfx", "ln") evaluatesTo (QName("pfx", "ln") : Identifier) withContext emptyContext }
      * - { QName("pfx", "ln") inContext (QName("pfx", "ln") -> "x") evaluatesTo ("x" : Identifier) withContext (QName("pfx", "ln") -> "x") }
      * - { QName("pfx", "ln") inContext (QName("pfx", "ln") -> Url("x")) evaluatesTo (Url("x") : Identifier) withContext (QName("pfx", "ln") -> Url("x")) }
      * - { QName("pfx", "ln") inContext (QName("pfx", "ln") -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) withContext (QName("pfx", "ln") -> QName("foo", "bar")) }
    }

    'valueExp - {
      * - { (StringLiteral("abc", false) : ValueExp) evaluatesTo (StringLiteral("abc", false) : ValueExp) withContext emptyContext }
      * - { (LocalName("a") : ValueExp) inContext ("a" -> "x") evaluatesTo ("x" : ValueExp) withContext ("a" -> "x") }
      * - { (LocalName("a") : ValueExp) inContext ("a" -> 42) evaluatesTo ("a" : ValueExp) withContext ("a" -> 42) }
      * - { (Url("a") : ValueExp) evaluatesTo (Url("a") : ValueExp) withContext emptyContext }
      * - { (QName("pfx", "ln") : ValueExp) evaluatesTo (QName("pfx", "ln") : ValueExp) withContext emptyContext }
      * - { (StringLiteral("abc", false) : ValueExp) evaluatesTo (StringLiteral("abc", false) : ValueExp) withContext emptyContext }
      * - { (MultiLineLiteral(Seq("abc", "def"), 4) : ValueExp) evaluatesTo (MultiLineLiteral(Seq("abc", "def"), 4) : ValueExp) withContext emptyContext }
      * - { (42 : ValueExp) evaluatesTo (42 : ValueExp) withContext emptyContext }
    }

    'valueExps - {
      * - {
        Seq[ValueExp](
          StringLiteral("abc", false),
          LocalName("a"),
          42
        ) evaluatesTo Seq[ValueExp](
          StringLiteral("abc", false),
          LocalName("a"),
          42
        ) withContext emptyContext
      }

      * - {
        Seq[ValueExp](
          StringLiteral("abc", false),
          LocalName("a"),
          42
        ) inContext (
          "a" -> "x"
          ) evaluatesTo Seq[ValueExp](
          StringLiteral("abc", false),
          LocalName("x"),
          42
        ) withContext (
          "a" -> "x"
          )
      }
    }

    'assignment - {
      'raw - {
        * - { ("a" -> "b": Assignment) evaluatesTo ("a" -> "b": Assignment) withContext emptyContext }

        * - { ("a" -> "b": Assignment) inContext ("a" -> "x") evaluatesTo ("x" -> "b": Assignment) withContext ("a" -> "x") }

        * - { ("a" -> "b": Assignment) inContext ("b" -> "y") evaluatesTo ("a" -> "y": Assignment) withContext ("b" -> "y") }
      }

      'bodyStmt - {
        * - { ("a" -> "b" : BodyStmt) evaluatesTo ("a" -> "b" : BodyStmt) withContext emptyContext }

        * - { ("a" -> "b" : BodyStmt) inContext("a" -> "x") evaluatesTo ("x" -> "b" : BodyStmt) withContext ("a" -> "x") }

        * - { ("a" -> "b" : BodyStmt) inContext("b" -> "y") evaluatesTo ("a" -> "y" : BodyStmt) withContext ("b" -> "y") }
      }

      'topLevel - {
        * - { ("a" -> "b" : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) withContext ("a" -> "b") }

        * - { ("a" -> "b" : TopLevel) inContext("a" -> "x") evaluatesTo (None : Option[TopLevel.InstanceExp]) withContext ("a" -> "x", "x" -> "b") }

        * - { ("a" -> "b" : TopLevel) inContext("b" -> "y") evaluatesTo (None : Option[TopLevel.InstanceExp]) withContext ("b" -> "y", "a" -> "y") }
      }
    }

    'assignments - {
      * - {
        Seq[TopLevel](
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None) withContext (
          "b" -> "c",
          "a" -> "c")
      }

      * - {
        Seq[TopLevel](
          "c" -> "d",
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None, None) withContext (
          "c" -> "d",
          "b" -> "d",
          "a" -> "d")
      }

      * - {
        Seq[TopLevel](
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "bx") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None, None) withContext (
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "dx")
      }
    }

    'tpeConstructor1 - {
      'rename - {
        * - { TpeConstructor1("X", Seq()) evaluatesTo TpeConstructor1("X", Seq()) withContext emptyContext }
        * - { TpeConstructor1("X", Seq()) inContext("a" -> "x") evaluatesTo TpeConstructor1("X", Seq()) withContext ("a" -> "x") }
        * - { TpeConstructor1("X", Seq()) inContext("X" -> "Y") evaluatesTo TpeConstructor1("Y", Seq()) withContext ("X" -> "Y") }
        * - { TpeConstructor1("X", Seq()) inContext(QName("ns", "X") -> "Y") evaluatesTo TpeConstructor1("Y", Seq()) withContext (QName("ns", "X") -> "Y") }
      }

      'expand_body - {
        * - {
          TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("a"),
            42)
          ) evaluatesTo TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("a"),
            42)
          ) withContext emptyContext
        }
        * - {
          TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("a"),
            42)
          ) inContext (
            "a" -> "x"
            ) evaluatesTo TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("x"),
            42)
            ) withContext (
            "a" -> "x"
            )
        }
      }

      'rename_and_expand - {
        TpeConstructor1("X", Seq(
          StringLiteral("abc", false),
          LocalName("a"),
          42)
        ) inContext (
          "a" -> "x",
          "X" -> "Y"
          ) evaluatesTo TpeConstructor1("Y", Seq(
          StringLiteral("abc", false),
          LocalName("x"),
          42)
          ) withContext (
          "a" -> "x",
          "X" -> "Y"
          )
      }
    }

    'tpeConstructorStar - {
      TpeConstructorStar evaluatesTo TpeConstructorStar withContext emptyContext
    }

    'tpeConstructor - {
      * - { (TpeConstructorStar : TpeConstructor) evaluatesTo (TpeConstructorStar : TpeConstructor) withContext emptyContext }

      * - {
        (TpeConstructor1("X", Seq(
          StringLiteral("abc", false),
          LocalName("a"),
          42)
        ) : TpeConstructor) inContext (
          "a" -> "x",
          "X" -> "Y"
          ) evaluatesTo (TpeConstructor1("Y", Seq(
          StringLiteral("abc", false),
          LocalName("x"),
          42)
        ) : TpeConstructor) withContext (
          "a" -> "x",
          "X" -> "Y"
          )
      }
    }

    'constructorDef - {
      TopLevel.ConstructorDef(
        "Foo",
        Seq(),
        ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
      ) evaluatesTo (None: Option[TopLevel.InstanceExp]) withContext (

        )
    }

    'constructorApp - {
      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) withContext emptyContext
      }
    }

//    'individuals - {
//      * - checkExpansion(parse("mySeq : Seq"), parse("mySeq : Seq"))
//
//      * - checkExpansion(
//        parse(
//          """mySeq : Seq
//          |  x = y""".stripMargin),
//        parse(
//          """mySeq : Seq
//          |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(
//        parse("Foo => Bar") ++
//          parse("foo : Foo"),
//        parse("foo : Bar"))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar
//            |  x = y
//          """.stripMargin) ++
//          parse("foo : Foo"),
//        parse(
//          """foo : Bar
//            |  x = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar()
//            |  x = y
//          """.stripMargin) ++
//          parse("foo : Foo"),
//        parse(
//          """foo : Bar
//            |  x = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar()
//            |  x = y
//          """.stripMargin) ++
//          parse("foo : Foo()"),
//        parse(
//          """foo : Bar
//            |  x = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar
//            |  x = y
//          """.stripMargin) ++
//          parse("foo : Foo()"),
//        parse(
//          """foo : Bar
//            |  x = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo(b) => Bar
//            |  a = b""".stripMargin) ++
//          parse("foo : Foo(y)"),
//        parse(
//          """foo : Bar
//            |  a = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar
//          """.stripMargin) ++
//          parse(
//            """foo : Foo
//              |  x = y""".stripMargin),
//        parse(
//          """foo : Bar
//              |  x = y""".stripMargin))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar
//           |Bar => Baz
//           |foo : Foo""".stripMargin),
//        parse(
//          "foo : Baz"))
//    }
//
//    'individualAssignments - {
//      * - checkExpansion(
//        parse(
//          """Foo = Bar
//            |foo : Foo
//          """.stripMargin),
//        parse(
//          """Foo = Bar
//            |foo : Bar""".stripMargin
//        ))
//
//      * - checkExpansion(
//        parse(
//          """Foo = Bar
//            |Bar => Baz
//            |foo : Foo
//          """.stripMargin),
//        parse(
//          """Foo = Bar
//            |foo : Baz""".stripMargin
//        ))
//
//      * - checkExpansion(
//        parse(
//          """Foo => Bar
//            |Bar = Baz
//            |foo : Foo
//          """.stripMargin),
//        parse(
//          """Bar = Baz
//            |foo : Baz""".stripMargin
//        ))
//    }
//
//    'imports - {
//
//      * - checkExpansion()(
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin),
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(Url("exampleImport") -> SBFile())(
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin),
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(Url("exampleImport") -> SBFile(tops = parse("""Foo => Bar""")))(
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin),
//        parse("""mySeq : Seq
//                |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(Url("exampleImport") -> SBFile(tops = parse("""Foo => Bar""")))(
//        parse("""import <exampleImport>
//                |mySeq : Seq
//                |  x = y""".stripMargin),
//        ProcessedImport(Url("exampleImport"), SBFile(Seq())) +:
//          parse("""mySeq : Seq
//                |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(Url("exampleImport") -> SBFile(tops = parse("""cat : Animal""")))(
//        parse("""import <exampleImport>
//                |mySeq : Seq
//                |  x = y""".stripMargin),
//        ProcessedImport(Url("exampleImport"), SBFile(parse("""cat : Animal"""))) +:
//          parse("""mySeq : Seq
//                  |  x = y""".stripMargin)
//      )
//
//      * - checkExpansion(Url("exampleImport") -> SBFile(tops = parse("""Foo => Bar""")))(
//        parse("""import <exampleImport>
//                |mySeq : Foo
//                |  x = y""".stripMargin),
//        ProcessedImport(Url("exampleImport"), SBFile(Seq())) +:
//          parse("""mySeq : Bar
//                  |  x = y""".stripMargin)
//      )
//
//    }
  }

}
