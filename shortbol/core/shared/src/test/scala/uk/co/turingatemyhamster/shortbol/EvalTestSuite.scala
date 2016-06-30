package uk.co.turingatemyhamster.shortbol

import fastparse.core.Parsed.Success
import fastparse.all._
import utest._

import scalaz._
import Scalaz._
import ast._
import ops._
import Eval.EvalOps
import ast.sugar._
import pragma.{ImportPragma, Resolver}
import ShortbolParser.POps



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
  def parse(shortbol: String): SBFile = parse(shortbol, ShortbolParser.SBFile)

  def parse[T](shortbol: String, p: Parser[T]):T =
    p.withPositions("_testcase_", shortbol) match {
      case s : Success[T] =>
        s.value
    }

  def parse_instances(shortbol: String): Seq[TopLevel.InstanceExp] =
    parse(shortbol).tops.collect { case i : TopLevel.InstanceExp => i }

  def parse_constructorDef(shortbol: String): ConstructorDef =
    ShortbolParser.ConstructorDef.parse(shortbol) match {
      case s: Success[ConstructorDef] =>
        s.value
    }

  val Ø = Fixture.emptyContext
  val ⊥ = null.asInstanceOf[EvalContext]

  implicit class TestOps[T](_t: T) {

    def in(c0: EvalContext) = new InContext(_t, c0)
    def evaluatesTo[U](u: U)(implicit e: EvalEval.Aux[T, U], an: AllNodes[U]) =
      (new InContext(_t, Ø)).evaluatesTo(u)
  }

  class InContext[T](t: T, c0: EvalContext) {
    def evaluatesWithRanges[U](expectedResult: U)(implicit eval: EvalEval.Aux[T, U], an: AllNodes[U]) = new Object {
      def in(expectedContext: EvalContext): Unit = {
        assert(eval != null)
        val (observedContext, observedResult) = eval(t).run(c0)

        if(expectedContext != ⊥) {
          val obsCtxt = observedContext.copy(logms = Seq())
          val expCtxt = expectedContext.copy(logms = Seq())
          assert(observedResult == expectedResult, obsCtxt == expCtxt)
        } else {
          assert(observedResult == expectedResult)
        }

        val without = AllNodes.in(observedResult) filter (_.region == null)
        assert(without.isEmpty)
      }
    }

    def evaluatesTo[U](expectedResult: U)(implicit eval: EvalEval.Aux[T, U]) = new Object {
      def in(expectedContext: EvalContext): Unit = {
        assert(eval != null)
        val (observedContext, observedResult) = eval(t).run(c0)

        if(expectedContext != ⊥) {
          val obsCtxt = observedContext.copy(logms = Seq())
          val expCtxt = expectedContext.copy(logms = Seq())
          assert(observedResult == expectedResult, obsCtxt == expCtxt)
        } else {
          assert(observedResult == expectedResult)
        }
      }
    }
  }

  val tests = TestSuite {

    'blankline - {
      * - { BlankLine() evaluatesTo BlankLine() in Ø }
      * - { (BlankLine() : BodyStmt) evaluatesTo (BlankLine() : BodyStmt) in Ø }
      * - { (BlankLine() : TopLevel) evaluatesTo (Nil : List[TopLevel.InstanceExp]) in Ø }
      * - { parse("", ShortbolParsers.BlankLine) evaluatesTo BlankLine() in Ø }
      * - { parse("", ShortbolParser.bodyStmt.BlankLine) evaluatesTo (BlankLine() : BodyStmt.BlankLine) in Ø }
    }

    'comment - {
      * - { Comment("a comment") evaluatesTo
        Comment("a comment") in Ø }
      * - { (Comment("a comment") : BodyStmt) evaluatesTo
        (Comment("a comment") : BodyStmt) in Ø }
      * - { (Comment("a comment") : TopLevel) evaluatesTo
        (Nil : List[TopLevel.InstanceExp]) in Ø }
      * - { parse("#a comment", ShortbolParsers.Comment) in Ø evaluatesWithRanges
        Comment("a comment") in Ø }
      * - { parse("#a comment", ShortbolParser.bodyStmt.Comment) in Ø evaluatesWithRanges
        (Comment("a comment") : BodyStmt.Comment) in Ø }
    }

    'literal - {
      * - { (StringLiteral.SingleLine("abc", false) : Literal) evaluatesTo
        (StringLiteral.SingleLine("abc", false) : Literal) in Ø }
      * - { parse("\"abc\"", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (StringLiteral.SingleLine("abc", false) : Literal) in Ø }
      * - { (StringLiteral.MultiLine(Seq("abc", "def"), 4) : Literal) evaluatesTo
        (StringLiteral.MultiLine(Seq("abc", "def"), 4) : Literal) in Ø }
      * - { parse("{\n    abc\n    def\n    }", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (StringLiteral.MultiLine(Seq("abc\n", "def\n"), 4) : Literal) in Ø }
      * - { (IntegerLiteral(42) : Literal) evaluatesTo
        (IntegerLiteral(42) : Literal) in Ø }
      * - { parse("42", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (IntegerLiteral(42) : Literal) in Ø }

    }

    'localName - {
      * - { LocalName("a") evaluatesTo ("a" : Identifier) in Ø }
      * - { parse("a", ShortbolParsers.Identifier) in Ø evaluatesWithRanges
        ("a" : Identifier) in Ø }

      * - { LocalName("a") in Ø.withAssignments("a" -> "x") evaluatesTo
        ("x" : Identifier) in Ø.withAssignments ("a" -> "x") }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments ("a" -> "x") }

      * - { LocalName("a") in Ø.withAssignments("a" -> Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments ("a" -> Url("x")) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments ("a" -> Url("x")) }

      * - { LocalName("a") in Ø.withAssignments("a" -> QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments ("a" -> QName("foo", "bar")) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments ("a" -> QName("foo", "bar")) }

      * - { LocalName("a") in Ø.withAssignments(QName("foo", "a") -> "x") evaluatesTo
        ("x" : Identifier) in Ø.withAssignments (QName("foo", "a") -> "x") }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("foo:a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments (QName("foo", "a") -> "x") }

      * - { LocalName("a") in Ø.withAssignments("a" -> 42) evaluatesTo
        ("a" : Identifier) in Ø.withAssignments ("a" -> 42) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = 42", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("a" : Identifier) in Ø.withAssignments ("a" -> 42) }
    }

    'url - {
      * - { Url("a") evaluatesTo (Url("a") : Identifier) in Ø }
      * - { parse("<a>", ShortbolParsers.Url) in Ø evaluatesWithRanges (Url("a") : Identifier) in Ø }

      * - { Url("a") in Ø.withAssignments(Url("a") -> "x") evaluatesTo ("x" : Identifier) in
        Ø.withAssignments (Url("a") -> "x") }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = x", ShortbolParsers.Assignment)) evaluatesWithRanges ("x" : Identifier) in
        Ø.withAssignments (Url("a") -> "x") }

      * - { Url("a") in Ø.withAssignments(Url("a") -> Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments (Url("a") -> Url("x")) }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments (Url("a") -> Url("x")) }

      * - { Url("a") in Ø.withAssignments(Url("a") -> QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (Url("a") -> QName("foo", "bar")) }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (Url("a") -> QName("foo", "bar")) }
    }

    'qname - {
      * - { QName("pfx", "ln") evaluatesTo (QName("pfx", "ln") : Identifier) in Ø }
      * - { parse("pfx:ln", ShortbolParsers.QName) in Ø evaluatesWithRanges
        (QName("pfx", "ln") : Identifier) in Ø }

      * - { QName("pfx", "ln") in Ø.withAssignments(QName("pfx", "ln") -> "x") evaluatesTo ("x" : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> "x") }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> "x") }

      * - { QName("pfx", "ln") in
        Ø.withAssignments(QName("pfx", "ln") -> Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> Url("x")) }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> Url("x")) }

      * - { QName("pfx", "ln") in
        Ø.withAssignments(QName("pfx", "ln") -> QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> QName("foo", "bar")) }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> QName("foo", "bar")) }
    }

    'valueExp - {
      * - { (StringLiteral.SingleLine("abc", false) : ValueExp) evaluatesTo
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }
      * - { parse("\"abc\"", ShortbolParsers.ValueExp) in Ø evaluatesWithRanges
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }

      * - { (LocalName("a") : ValueExp) in
        Ø.withAssignments("a" -> "x") evaluatesTo ("x" : ValueExp) in Ø.withAssignments ("a" -> "x") }
      * - { parse("a", ShortbolParsers.ValueExp) in
        Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : ValueExp) in Ø.withAssignments ("a" -> "x") }

      * - { (LocalName("a") : ValueExp) in
        Ø.withAssignments("a" -> 42) evaluatesTo (42 : ValueExp) in Ø.withAssignments ("a" -> 42) }
      * - { parse("a", ShortbolParsers.ValueExp) in
        Ø.withAssignments(parse("a = 42", ShortbolParsers.Assignment)) evaluatesWithRanges
        (42 : ValueExp) in Ø.withAssignments ("a" -> 42) }

      * - { (Url("a") : ValueExp) evaluatesTo
        (Url("a") : ValueExp) in Ø }
      * - { parse("<a>", ShortbolParsers.ValueExp) in Ø evaluatesWithRanges
        (Url("a") : ValueExp) in Ø }

      * - { (QName("pfx", "ln") : ValueExp) evaluatesTo
        (QName("pfx", "ln") : ValueExp) in Ø }
      * - { parse("pfx:ln", ShortbolParsers.ValueExp) in Ø evaluatesWithRanges
        (QName("pfx", "ln") : ValueExp) in Ø }

      * - { (StringLiteral.SingleLine("abc", false) : ValueExp) evaluatesTo
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }
      * - { parse("\"abc\"", ShortbolParsers.ValueExp) in Ø evaluatesWithRanges
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }

      * - { (StringLiteral.MultiLine(Seq("abc", "def"), 4) : ValueExp) evaluatesTo
        (StringLiteral.MultiLine(Seq("abc", "def"), 4) : ValueExp) in Ø }

      * - { (42 : ValueExp) evaluatesTo (42 : ValueExp) in Ø }
    }

    'valueExps - {
      * - {
        Seq[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) evaluatesTo Seq[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) in Ø
      }

      * - {
        Seq[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) in Ø.withAssignments(
          "a" -> "x"
          ) evaluatesTo Seq[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("x"),
          42
        ) in Ø.withAssignments (
          "a" -> "x"
          )
      }
    }

    'assignment - {
      'raw - {
        * - { ("a" -> "b": Assignment) evaluatesTo ("a" -> "b": Assignment) in Ø }
        * - { parse("a = b", ShortbolParsers.Assignment) in Ø evaluatesWithRanges  ("a" -> "b": Assignment) in Ø }

        * - { ("a" -> "b": Assignment) in
          Ø.withAssignments("a" -> "x") evaluatesTo
          ("x" -> "b": Assignment) in Ø.withAssignments ("a" -> "x") }
        * - { parse("a = b", ShortbolParsers.Assignment) in
          Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
          ("x" -> "b": Assignment) in Ø.withAssignments ("a" -> "x") }

        * - { ("a" -> "b": Assignment) in
          Ø.withAssignments("b" -> "y") evaluatesTo
          ("a" -> "y": Assignment) in Ø.withAssignments ("b" -> "y") }
        * - { parse("a = b", ShortbolParsers.Assignment) in
          Ø.withAssignments(parse("b = y", ShortbolParsers.Assignment)) evaluatesWithRanges
          ("a" -> "y": Assignment) in Ø.withAssignments ("b" -> "y") }
      }

      'bodyStmt - {
        * - { ("a" -> "b" : BodyStmt) evaluatesTo ("a" -> "b" : BodyStmt) in Ø }

        * - { ("a" -> "b" : BodyStmt) in Ø.withAssignments("a" -> "x") evaluatesTo ("x" -> "b" : BodyStmt) in Ø.withAssignments ("a" -> "x") }

        * - { ("a" -> "b" : BodyStmt) in Ø.withAssignments("b" -> "y") evaluatesTo ("a" -> "y" : BodyStmt) in Ø.withAssignments ("b" -> "y") }
      }

      'topLevel - {
        * - { ("a" -> "b" : TopLevel) evaluatesTo (Nil : List[TopLevel.InstanceExp]) in Ø.withAssignments ("a" -> "b") }

        // withAssignments with matching keys doesn't merge values
        * - { ("a" -> "b" : TopLevel) in Ø.withAssignments("a" -> "x") evaluatesTo (Nil : List[TopLevel.InstanceExp]) in Ø.withAssignments("a" -> "x").withAssignments("a" -> "b") }

        * - { ("a" -> "b" : TopLevel) in Ø.withAssignments("b" -> "y") evaluatesTo (Nil : List[TopLevel.InstanceExp]) in Ø.withAssignments ("b" -> "y", "a" -> "b") }
      }
    }

    'assignments - {
      * - {
        * - { Seq[TopLevel](
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[List[TopLevel.InstanceExp]](Nil, Nil) in Ø.withAssignments (
          "b" -> "c",
          "a" -> "b") }
//
//        * - { (LocalName("a") : Identifier) evaluatesTo (LocalName("c") : Identifier) in Ø.withAssignments (
//          "b" -> "c",
//          "a" -> "b") }
//
//        * - { (LocalName("b") : Identifier) evaluatesTo (LocalName("c") : Identifier) in Ø.withAssignments (
//          "b" -> "c",
//          "a" -> "b") }
//
//        * - { (LocalName("c") : Identifier) evaluatesTo (LocalName("c") : Identifier) in Ø.withAssignments (
//          "b" -> "c",
//          "a" -> "b") }
      }

      * - {
        Seq[TopLevel](
          "c" -> "d",
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[List[TopLevel.InstanceExp]](Nil, Nil, Nil) in Ø.withAssignments (
          "c" -> "d",
          "b" -> "c",
          "a" -> "b")
      }

      * - {
        Seq[TopLevel](
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "bx") evaluatesTo Seq[List[TopLevel.InstanceExp]](Nil, Nil, Nil) in Ø.withAssignments (
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "bx")
      }
    }

    'tpeConstructor1 - {
      'rename - {
        * - {
          TpeConstructor1("X", Seq()) evaluatesTo TpeConstructor1("X", Seq()) in Ø
        }
        * - {
          TpeConstructor1("X", Seq()) in Ø.withAssignments("a" -> "x") evaluatesTo TpeConstructor1("X", Seq()) in Ø.withAssignments ("a" -> "x") }
        * - { TpeConstructor1("X", Seq()) in Ø.withAssignments("X" -> "Y") evaluatesTo TpeConstructor1("Y", Seq()) in Ø.withAssignments ("X" -> "Y") }
        * - { TpeConstructor1("X", Seq()) in Ø.withAssignments(QName("ns", "X") -> "Y") evaluatesTo TpeConstructor1("Y", Seq()) in Ø.withAssignments (QName("ns", "X") -> "Y") }
      }

      'expand_body - {
        * - {
          TpeConstructor1("X", Seq(
            StringLiteral.SingleLine("abc", false),
            LocalName("a"),
            42)
          ) evaluatesTo (
            (TpeConstructor1("X", Seq(
              StringLiteral.SingleLine("abc", false),
              LocalName("a"),
              42)) : TpeConstructor) ->
            Seq[BodyStmt]()
            ) in Ø
        }
        * - {
          TpeConstructor1("X", Seq(
            StringLiteral.SingleLine("abc", false),
            LocalName("a"),
            42)
          ) in Ø.withAssignments (
            "a" -> "x"
            ) evaluatesTo TpeConstructor1("X", Seq(
            StringLiteral.SingleLine("abc", false),
            LocalName("x"),
            42)
            ) in Ø.withAssignments (
            "a" -> "x"
            )
        }
      }

      'rename_and_expand - {
        TpeConstructor1("Foo", Seq(
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42)
        ) in Ø.withAssignments (
          "a" -> "x",
          "Foo" -> "Bar"
          ) evaluatesTo (
          (TpeConstructor1("Foo", Seq(
          StringLiteral.SingleLine("abc", false),
          LocalName("x"),
          42)) : TpeConstructor) ->
          Seq[BodyStmt]()
          ) in Ø.withAssignments (
          "a" -> "x",
          "Foo" -> "Bar"
          )
      }
    }

    'tpeConstructorStar - {
      TpeConstructorStar() evaluatesTo (TpeConstructorStar(), Seq.empty[BodyStmt]) in Ø
    }

    'tpeConstructor - {
      * - { (TpeConstructorStar() : TpeConstructor) evaluatesTo ((TpeConstructorStar() : TpeConstructor, Seq.empty[BodyStmt])) in Ø }

      * - {
        (TpeConstructor1("X", Seq(
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42)
        ) : TpeConstructor) in Ø.withAssignments(
          "a" -> "x",
          "X" -> "Y"
          ) evaluatesTo (TpeConstructor1("Y", Seq(
          StringLiteral.SingleLine("abc", false),
          LocalName("x"),
          42)
        ) : TpeConstructor) in Ø.withAssignments (
          "a" -> "x",
          "X" -> "Y"
          )
      }
    }

    'tl_constructorDef - {
      * - {
        TopLevel.ConstructorDef(
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ) evaluatesTo (Nil: List[TopLevel.InstanceExp]) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
          )
      }

      * - {
        TopLevel.ConstructorDef(
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ) in Ø.withAssignments(
          "Foo" -> "Bar"
        ) evaluatesTo (Nil: List[TopLevel.InstanceExp]) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ).withAssignments(
          "Foo" -> "Bar"
        )
      }
    }

    'constructorApp - {
      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) in Ø
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Bar", Seq()),
          Seq()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq("a", "b")),
          Seq()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Bar", Seq()),
          Seq()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq("a" -> "b")
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Bar", Seq()),
          Seq("a" -> "b")
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq()),
          Seq()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq("x" -> "y"))
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("Bar", Seq()),
          Seq("x" -> "y")
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq(),
            ConstructorApp(TpeConstructor1("Bar", Seq()), Seq("x" -> "y"))
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq("matthew", 40)),
          Seq()
        ) in Ø.withConstructors(
          ConstructorDef(
            "Foo",
            Seq("name", "age"),
            ConstructorApp(TpeConstructor1(("foaf" :# "Person"), Seq()), Seq(
              ("foaf" :# "name") -> "name",
              ("foaf" :# "age") -> "age"
            ))
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("foaf" :# "Person", Seq()), Seq(
            ("foaf" :# "name") -> "matthew",
            ("foaf" :# "age") -> 40)
        ) in Ø.withConstructors(
          ConstructorDef(
            "Foo",
            Seq("name", "age"),
            ConstructorApp(TpeConstructor1(("foaf" :# "Person"), Seq()), Seq(
              ("foaf" :# "name") -> "name",
              ("foaf" :# "age") -> "age"
            ))
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", Seq("matthew", 40)),
          Seq(("foaf" :# "knows") -> "caroline")
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq("name", "a"),
            ConstructorApp(TpeConstructor1(("Bar"), Seq("a")), Seq(
              ("foaf" :# "name") -> "name"
            ))
          ),
          ConstructorDef(
            "Bar",
            Seq("age"),
            ConstructorApp(TpeConstructor1(("foaf" :# "Person"), Seq()), Seq(
              ("foaf" :# "age") -> "age"
            ))
          )
        ) evaluatesTo ConstructorApp(
          TpeConstructor1("foaf" :# "Person", Seq()), Seq(
            ("foaf" :# "age") -> 40,
            ("foaf" :# "name") -> "matthew",
            ("foaf" :# "knows") -> "caroline")
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            Seq("name", "a"),
            ConstructorApp(TpeConstructor1(("Bar"), Seq("a")), Seq(
              ("foaf" :# "name") -> "name"
            ))
          ),
          ConstructorDef(
            "Bar",
            Seq("age"),
            ConstructorApp(TpeConstructor1(("foaf" :# "Person"), Seq()), Seq(
              ("foaf" :# "age") -> "age"
            ))
          )
        )
      }
    }

    'sbfile - {
      parse(
        """WithNameAge(name, age) => WithAge(age)
          |  foaf:name = name
          |
          |WithAge(age) => foaf:person
          |  foaf:age = age
          |
          |me : WithNameAge("matthew", 40)
          |  foaf:knows = "caroline"
          |""".stripMargin) in Ø evaluatesWithRanges parse_instances(
        """me : foaf:person
          |  foaf:age = 40
          |  foaf:name = "matthew"
          |  foaf:knows = "caroline"
          |""".stripMargin) in Ø.withConstructors (
        parse_constructorDef(
          """WithNameAge(name, age) => WithAge(age)
            |  foaf:name = name
            |""".stripMargin),
        parse_constructorDef(
          """WithAge(age) => foaf:person
            |  foaf:age = age
            |""".stripMargin)
      ).withInstances(
        parse_instances(
          """me : foaf:person
            |  foaf:age = 40
            |  foaf:name = "matthew"
            |  foaf:knows = "caroline"
            |""".stripMargin).map { i => i.instanceExp} :_*)
    }

    'instance_reassignment - {
      val (ctxt, res) = parse(
        """a : x
          |a : y
          |a : z""".stripMargin).eval.run(Ø)
      val aRes = ctxt.resolveInst("a")
      assert(aRes == Some(InstanceExp("a", ConstructorApp(TpeConstructor1("z", Seq()), Seq()))))
      assert(ctxt.insts("a").length == 3)
    }
  }
}
