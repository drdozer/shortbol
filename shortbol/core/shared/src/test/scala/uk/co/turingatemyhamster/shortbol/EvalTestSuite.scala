package uk.co.turingatemyhamster.shortbol

import fastparse.core.Parsed.Success
import fastparse.all._
import utest._

import scalaz._
import Scalaz._
import shorthandAst._
import ops._
import Eval.EvalOps
import sharedAst._
import sharedAst.sugar._
import shorthandAst.sugar._
import pragma.{ImportPragma, Resolver}
import ShortbolParser.POps
import longhandAst.sugar._
import uk.co.turingatemyhamster.shortbol.terms.FOAF


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

  def parseLB(shortbol: String): longhandAst.SBFile = longhandAst.SBFile(parse_instances(shortbol))

  def parse_instances(shortbol: String): List[longhandAst.InstanceExp] = {
    def process(ca: shorthandAst.ConstructorApp): longhandAst.ConstructorApp = ca match {
      case ConstructorApp(TpeConstructor1(tpe, List()), body) =>
        longhandAst.ConstructorApp(longhandAst.TpeConstructor(tpe),
          body collect {
            case shorthandAst.BodyStmt.PropertyExp(shorthandAst.PropertyExp(p, v)) =>
              longhandAst.PropertyExp(p, v match {
                case shorthandAst.PropertyValue.Literal(l) => longhandAst.PropertyValue.Literal(l)
                case shorthandAst.PropertyValue.Reference(r) => longhandAst.PropertyValue.Reference(r)
                case shorthandAst.PropertyValue.Nested(n) => process(n)
              })
          })
    }

    parse(shortbol).tops.collect {
      case TopLevel.InstanceExp(InstanceExp(identifier, ca)) =>

        longhandAst.InstanceExp(identifier, process(ca)) }
  }

  def parse_instances_eval(shortbol: String): longhandAst.SBFile =
    longhandAst.SBFile(parse_instances(shortbol))

  def parse_constructorDef(shortbol: String): ConstructorDef =
    ShortbolParser.ConstructorDef.parse(shortbol) match {
      case s: Success[ConstructorDef] =>
        s.value
    }

  val Ø = Fixture.emptyContext
  val ⊥ = null.asInstanceOf[EvalContext]

  implicit class TestOps[T](_t: T) {

    def in(c0: EvalContext) = new InContext(_t, c0)
    def evaluatesTo[U, UU](u: UU)(implicit e: Eval.Aux[T, U], an: AllNodes[U], ev: UU => U) =
      (new InContext(_t, Ø)).evaluatesTo(u)
  }

  class InContext[T](t: T, c0: EvalContext) {
    def evaluatesWithRanges[U](expectedResult: U)(implicit eval: Eval.Aux[T, U], an: AllNodes[U]) = new Object {
      def in(expectedContext: EvalContext): Unit = {
        assert(eval != null)
        val (observedContext, observedResult) = eval(t).run(c0)

        if(expectedContext != ⊥) {
          val obsCtxt = observedContext.copy(logms = List())
          val expCtxt = expectedContext.copy(logms = List())
          assert(observedResult == expectedResult, obsCtxt == expCtxt)
        } else {
          assert(observedResult == expectedResult)
        }

        val without = AllNodes.in(observedResult) filter (_.region == null)
        assert(without.isEmpty)
      }
    }

    def evaluatesTo[U, UU](expectedResultU: UU)(implicit eval: Eval.Aux[T, U], ev: UU => U) = new Object {
      def in(expectedContext: EvalContext): Unit = {
        assert(eval != null)
        val (observedContext, observedResult) = eval(t).run(c0)
        val expectedResult = ev(expectedResultU)

        if(expectedContext != ⊥) {
          val obsCtxt = observedContext.copy(logms = List())
          val expCtxt = expectedContext.copy(logms = List())
          assert(observedResult == expectedResult, obsCtxt == expCtxt)
        } else {
          assert(observedResult == expectedResult)
        }
      }
    }
  }

  val tests = TestSuite {

    'blankline - {
      * - { (BlankLine() : TopLevel) evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø }
      * - { parse("", ShortbolParser.BodyStmt) evaluatesTo List.empty[longhandAst.PropertyExp] in Ø }
    }

    'comment - {
      * - { (Comment("a comment") : TopLevel) evaluatesTo
        (List.empty[longhandAst.InstanceExp]) in Ø }
      * - { parse("#a comment", ShortbolParser.BodyStmt) in Ø evaluatesWithRanges
        List.empty[longhandAst.PropertyExp] in Ø }
    }

    'literal - {
      * - { (StringLiteral.SingleLine("abc", false) : Literal) evaluatesTo
        (StringLiteral.SingleLine("abc", false) : Literal) in Ø }
      * - { parse("\"abc\"", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (StringLiteral.SingleLine("abc", false) : Literal) in Ø }
      * - { (StringLiteral.MultiLine(List("abc", "def"), 4) : Literal) evaluatesTo
        (StringLiteral.MultiLine(List("abc", "def"), 4) : Literal) in Ø }
      * - { parse("{\n    abc\n    def\n    }", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (StringLiteral.MultiLine(List("abc\n", "def\n"), 4) : Literal) in Ø }
      * - { (IntegerLiteral(42) : Literal) evaluatesTo
        (IntegerLiteral(42) : Literal) in Ø }
      * - { parse("42", ShortbolParsers.Literal) in Ø evaluatesWithRanges
        (IntegerLiteral(42) : Literal) in Ø }

    }

    'localName - {
      * - { LocalName("a") evaluatesTo ("a" : Identifier) in Ø }
      * - { parse("a", ShortbolParsers.Identifier) in Ø evaluatesWithRanges
        ("a" : Identifier) in Ø }

      * - { LocalName("a") in Ø.withAssignments("a" := "x") evaluatesTo
        ("x" : Identifier) in Ø.withAssignments ("a" := "x") }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments ("a" := "x") }

      * - { LocalName("a") in Ø.withAssignments("a" := Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments ("a" := Url("x")) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments ("a" := Url("x")) }

      * - { LocalName("a") in Ø.withAssignments("a" := QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments ("a" := QName("foo", "bar")) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments ("a" := QName("foo", "bar")) }

      * - { LocalName("a") in Ø.withAssignments(QName("foo", "a") := "x") evaluatesTo
        ("x" : Identifier) in Ø.withAssignments (QName("foo", "a") := "x") }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("foo:a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments (QName("foo", "a") := "x") }

      * - { LocalName("a") in Ø.withAssignments("a" := 42) evaluatesTo
        ("a" : Identifier) in Ø.withAssignments ("a" := 42) }
      * - { parse("a", ShortbolParsers.Identifier) in
        Ø.withAssignments(parse("a = 42", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("a" : Identifier) in Ø.withAssignments ("a" := 42) }
    }

    'url - {
      * - { Url("a") evaluatesTo (Url("a") : Identifier) in Ø }
      * - { parse("<a>", ShortbolParsers.Url) in Ø evaluatesWithRanges (Url("a") : Identifier) in Ø }

      * - { Url("a") in Ø.withAssignments(Url("a") := "x") evaluatesTo ("x" : Identifier) in
        Ø.withAssignments (Url("a") := "x") }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = x", ShortbolParsers.Assignment)) evaluatesWithRanges ("x" : Identifier) in
        Ø.withAssignments (Url("a") := "x") }

      * - { Url("a") in Ø.withAssignments(Url("a") := Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments (Url("a") := Url("x")) }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments (Url("a") := Url("x")) }

      * - { Url("a") in Ø.withAssignments(Url("a") := QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (Url("a") := QName("foo", "bar")) }
      * - { parse("<a>", ShortbolParsers.Url) in
        Ø.withAssignments(parse("<a> = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (Url("a") := QName("foo", "bar")) }
    }

    'qname - {
      * - { QName("pfx", "ln") evaluatesTo (QName("pfx", "ln") : Identifier) in Ø }
      * - { parse("pfx:ln", ShortbolParsers.QName) in Ø evaluatesWithRanges
        (QName("pfx", "ln") : Identifier) in Ø }

      * - { QName("pfx", "ln") in Ø.withAssignments(QName("pfx", "ln") := "x") evaluatesTo
        ("x" : Identifier) in Ø.withAssignments (QName("pfx", "ln") := "x") }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : Identifier) in Ø.withAssignments (QName("pfx", "ln") := "x") }

      * - { QName("pfx", "ln") in
        Ø.withAssignments(QName("pfx", "ln") := Url("x")) evaluatesTo
        (Url("x") : Identifier) in Ø.withAssignments (QName("pfx", "ln") := Url("x")) }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = <x>", ShortbolParsers.Assignment)) evaluatesWithRanges
        (Url("x") : Identifier) in Ø.withAssignments (QName("pfx", "ln") := Url("x")) }

      * - { QName("pfx", "ln") in
        Ø.withAssignments(QName("pfx", "ln") := QName("foo", "bar")) evaluatesTo
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (QName("pfx", "ln") := QName("foo", "bar")) }
      * - { parse("pfx:ln", ShortbolParsers.QName) in
        Ø.withAssignments(parse("pfx:ln = foo:bar", ShortbolParsers.Assignment)) evaluatesWithRanges
        (QName("foo", "bar") : Identifier) in Ø.withAssignments (QName("pfx", "ln") := QName("foo", "bar")) }
    }

    'valueExp - {
      * - { (StringLiteral.SingleLine("abc", false) : ValueExp) evaluatesTo
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }
      * - { parse("\"abc\"", ShortbolParsers.ValueExp) in Ø evaluatesWithRanges
        (StringLiteral.SingleLine("abc", false) : ValueExp) in Ø }

      * - { (LocalName("a") : ValueExp) in
        Ø.withAssignments("a" := "x") evaluatesTo ("x" : ValueExp) in Ø.withAssignments ("a" := "x") }
      * - { parse("a", ShortbolParsers.ValueExp) in
        Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
        ("x" : ValueExp) in Ø.withAssignments ("a" := "x") }

      * - { (LocalName("a") : ValueExp) in
        Ø.withAssignments("a" := 42) evaluatesTo (42 : ValueExp) in Ø.withAssignments ("a" := 42) }
      * - { parse("a", ShortbolParsers.ValueExp) in
        Ø.withAssignments(parse("a = 42", ShortbolParsers.Assignment)) evaluatesWithRanges
        (42 : ValueExp) in Ø.withAssignments ("a" := 42) }

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

      * - { (StringLiteral.MultiLine(List("abc", "def"), 4) : ValueExp) evaluatesTo
        (StringLiteral.MultiLine(List("abc", "def"), 4) : ValueExp) in Ø }

      * - { (42 : ValueExp) evaluatesTo (42 : ValueExp) in Ø }
    }

    'qnameLookup - {

      'nothing_in_scope - {
        parse("""me : foaf:person
          |  name = "matthew"""".stripMargin) evaluatesTo parse_instances_eval(
          """me : foaf:person
             |  name = "matthew"""".stripMargin) in ⊥
      }

      'name_in_scope - {
        val ctxt = parse("""foaf:name : predicate""").eval.exec(Ø)

        val qnams = ctxt.qnams
        assert(qnams.nonEmpty)
        assert(qnams.get("name").nonEmpty)
        assert(qnams("name").contains(FOAF.Core.name))

        parse("""me : foaf:person
                  |  name = "matthew"""".stripMargin) in ctxt evaluatesTo
          parse_instances_eval(
            """me : foaf:person
              |  foaf:name = "matthew"""".stripMargin) in ⊥
      }

    }

    'valueExps - {
      * - {
        List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) evaluatesTo List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) in Ø
      }

      * - {
        List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) in Ø.withAssignments(
          "a" := "x"
          ) evaluatesTo List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("x"),
          42
        ) in Ø.withAssignments (
          "a" := "x"
          )
      }

      * - {
        List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42
        ) in Ø.withAssignments(
          "a" := 11
          ) evaluatesTo List[ValueExp](
          StringLiteral.SingleLine("abc", false),
          11,
          42
        ) in Ø.withAssignments (
          "a" := 11
          )
      }
    }

    'propertyExp - {
      'literal - {
        'raw - {
          ("a" := 42 : PropertyExp) evaluatesTo ("a" := 42).head in Ø
        }

        'renamed - {
          ("a" := 42 : PropertyExp) in
            Ø.withAssignments ("a" := "x") evaluatesTo
            ("x" := 42).head in
            Ø.withAssignments ("a" := "x")
        }
      }

      'reference - {
        'raw - {
          ("a" := "b" : PropertyExp) evaluatesTo ("a" := "b").head in Ø
        }

        'renamed - {
          ("a" := "b" : PropertyExp) in
            Ø.withAssignments ("a" := "x") evaluatesTo
            ("x" := "b").head in
            Ø.withAssignments ("a" := "x")
        }

        'value_reference - {
          ("a" := "b" : PropertyExp) in
            Ø.withAssignments ("b" := "x") evaluatesTo
            ("a" := "x").head in
            Ø.withAssignments ("b" := "x")
        }

        'value_literal - {
          ("a" := "b" : PropertyExp) in
            Ø.withAssignments ("b" := 42) evaluatesTo
            ("a" := 42).head in
            Ø.withAssignments ("b" := 42)
        }
      }




    }

    'assignment - {
      'raw - {
        * - { ("a" := "b": Assignment) evaluatesTo ("a" := "b": Assignment) in Ø }
        * - { parse("a = b", ShortbolParsers.Assignment) in Ø evaluatesWithRanges  ("a" := "b": Assignment) in Ø }

        * - { ("a" := "b": Assignment) in
          Ø.withAssignments("a" := "x") evaluatesTo
          ("x" := "b": Assignment) in Ø.withAssignments ("a" := "x") }
        * - { parse("a = b", ShortbolParsers.Assignment) in
          Ø.withAssignments(parse("a = x", ShortbolParsers.Assignment)) evaluatesWithRanges
          ("x" := "b": Assignment) in Ø.withAssignments ("a" := "x") }

        * - { ("a" := "b": Assignment) in
          Ø.withAssignments("b" := "y") evaluatesTo
          ("a" := "y": Assignment) in Ø.withAssignments ("b" := "y") }
        * - { parse("a = b", ShortbolParsers.Assignment) in
          Ø.withAssignments(parse("b = y", ShortbolParsers.Assignment)) evaluatesWithRanges
          ("a" := "y": Assignment) in Ø.withAssignments ("b" := "y") }
      }

      'bodyStmt - {
        * - { ("a" := "b" : BodyStmt) evaluatesTo ("a" := "b") in Ø }

        * - { ("a" := "b" : BodyStmt) in Ø.withAssignments("a" := "x") evaluatesTo
          ("x" := "b") in Ø.withAssignments ("a" := "x") }

        * - { ("a" := "b" : BodyStmt) in Ø.withAssignments("b" := "y") evaluatesTo
          ("a" := "y") in Ø.withAssignments ("b" := "y") }
      }

      'topLevel - {
        * - { ("a" := "b" : TopLevel) evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø.withAssignments ("a" := "b") }

        // withAssignments with matching keys doesn't merge values
        * - { ("a" := "b" : TopLevel) in Ø.withAssignments("a" := "x") evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø.withAssignments("a" := "x").withAssignments("a" := "b") }

        * - { ("a" := "b" : TopLevel) in Ø.withAssignments("b" := "y") evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø.withAssignments ("b" := "y", "a" := "b") }
      }
    }

    'assignments - {
      * - {
        List[TopLevel](
          "b" := "c",
          "a" := "b") evaluatesTo List[List[longhandAst.InstanceExp]](Nil, Nil) in Ø.withAssignments (
          "b" := "c",
          "a" := "b")
      }

      * - {
        List[TopLevel](
          "c" := "d",
          "b" := "c",
          "a" := "b") evaluatesTo List[List[longhandAst.InstanceExp]](Nil, Nil, Nil) in Ø.withAssignments (
          "c" := "d",
          "b" := "c",
          "a" := "b")
      }

      * - {
        List[TopLevel](
          "bx" := "cx",
          "cx" := "dx",
          "ax" := "bx") evaluatesTo List[List[longhandAst.InstanceExp]](Nil, Nil, Nil) in Ø.withAssignments (
          "bx" := "cx",
          "cx" := "dx",
          "ax" := "bx")
      }
    }

    'tpeConstructor1 - {
      'rename - {
        * - {
          TpeConstructor1("X", List()) evaluatesTo
            (longhandAst.TpeConstructor("X"), List.empty[longhandAst.PropertyExp]) in Ø
        }
        * - {
          TpeConstructor1("X", List()) in
            Ø.withAssignments("a" := "x") evaluatesTo
            (longhandAst.TpeConstructor("X"), List.empty[longhandAst.PropertyExp]) in
            Ø.withAssignments ("a" := "x")
        }
        * - {
          TpeConstructor1("X", List()) in
            Ø.withAssignments("X" := "Y") evaluatesTo
            (longhandAst.TpeConstructor("Y"), List.empty[longhandAst.PropertyExp]) in
            Ø.withAssignments ("X" := "Y") }
        * - {
          TpeConstructor1("X", List()) in
            Ø.withAssignments(QName("ns", "X") := "Y") evaluatesTo
            (longhandAst.TpeConstructor("Y"), List.empty[longhandAst.PropertyExp]) in
            Ø.withAssignments (QName("ns", "X") := "Y") }
      }

      'expand_body - {
        * - {
          TpeConstructor1("X", List(
            StringLiteral.SingleLine("abc", false),
            LocalName("a"),
            42)
          ) evaluatesTo (
            longhandAst.TpeConstructor("X") ->
              List[longhandAst.PropertyExp]()
            ) in Ø
        }

        * - {
          TpeConstructor1("X", List(
            StringLiteral.SingleLine("abc", false),
            LocalName("a"),
            42)
          ) in Ø.withAssignments (
            "a" := "x"
          ) evaluatesTo (
            longhandAst.TpeConstructor("X"),
            List.empty[longhandAst.PropertyExp]
          ) in Ø.withAssignments (
            "a" := "x"
            )
        }
      }

      'rename_and_expand - {
        TpeConstructor1("Foo", List(
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42)
        ) in Ø.withAssignments (
          "a" := "x",
          "Foo" := "Bar"
        ) evaluatesTo (
          longhandAst.TpeConstructor("Bar") ->
            List.empty[longhandAst.PropertyExp]
          ) in Ø.withAssignments (
          "a" := "x",
          "Foo" := "Bar"
        )
      }
    }

//    'tpeConstructorStar - {
//      TpeConstructorStar() evaluatesTo (TpeConstructorStar() : TpeConstructor, List.empty[BodyStmt]) in Ø
//    }

    'tpeConstructor - {
//      * - { (TpeConstructorStar() : TpeConstructor) evaluatesTo ((TpeConstructorStar() : TpeConstructor, List.empty[BodyStmt])) in Ø }

      * - {
        (TpeConstructor1("X", List(
          StringLiteral.SingleLine("abc", false),
          LocalName("a"),
          42)
        ) : TpeConstructor) in Ø.withAssignments(
          "a" := "x",
          "X" := "Y"
          ) evaluatesTo (
          longhandAst.TpeConstructor("Y"), List.empty[longhandAst.PropertyExp]) in
          Ø.withAssignments (
          "a" := "x",
          "X" := "Y"
          )
      }
    }

    'tl_constructorDef - {
      * - {
        TopLevel.ConstructorDef(
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ) evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        )
      }

      * - {
        TopLevel.ConstructorDef(
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ) in Ø.withAssignments(
          "Foo" := "Bar"
        ) evaluatesTo (List.empty[longhandAst.InstanceExp]) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ).withAssignments(
          "Foo" := "Bar"
        )
      }
    }

    'constructorApp - {
      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List()),
          List()
        ) evaluatesTo longhandAst.ConstructorApp(
          longhandAst.TpeConstructor("Foo"),
          List()
        ) in Ø
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List()),
          List()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ) evaluatesTo longhandAst.ConstructorApp(
          longhandAst.TpeConstructor("Bar"),
          List()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List("a", "b")),
          List()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ) evaluatesTo longhandAst.ConstructorApp(
          longhandAst.TpeConstructor("Bar"),
          List()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List()),
          "a" := "b"
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        ) evaluatesTo longhandAst.ConstructorApp(
          longhandAst.TpeConstructor("Bar"),
          "a" := "b"
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), List())
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List()),
          List()
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), "x" := "y")
          )
        ) evaluatesTo longhandAst.ConstructorApp(
          longhandAst.TpeConstructor("Bar"),
          "x" := "y"
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List(),
            ConstructorApp(TpeConstructor1("Bar", List()), "x" := "y")
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List("matthew", 40)),
          List()
        ) in Ø.withConstructors(
          ConstructorDef(
            "Foo",
            List("name", "age"),
            ConstructorApp(TpeConstructor1(FOAF.Core.Person, List()),
              FOAF.Core.name := "name",
              FOAF.Core.age := "age"
            )
          )
        ) evaluatesTo FOAF.Core.Person(
            FOAF.Core.name := "matthew",
            FOAF.Core.age := 40
        ) in Ø.withConstructors(
          ConstructorDef(
            "Foo",
            List("name", "age"),
            ConstructorApp(TpeConstructor1(FOAF.Core.Person, List()),
              FOAF.Core.name := "name",
              FOAF.Core.age := "age"
            )
          )
        )
      }

      * - {
        ConstructorApp(
          TpeConstructor1("Foo", List("matthew", 40)),
          FOAF.Core.knows := "caroline"
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List("name", "a"),
            ConstructorApp(TpeConstructor1("Bar", List("a")),
              FOAF.Core.name := "name"
            )
          ),
          ConstructorDef(
            "Bar",
            List("age"),
            ConstructorApp(TpeConstructor1(FOAF.Core.Person, List()),
              FOAF.Core.age := "age"
            )
          )
        ) evaluatesTo FOAF.Core.Person(
          FOAF.Core.age := 40,
          FOAF.Core.name := "matthew",
          FOAF.Core.knows := "caroline"
        ) in Ø.withConstructors (
          ConstructorDef(
            "Foo",
            List("name", "a"),
            ConstructorApp(TpeConstructor1(("Bar"), List("a")),
              FOAF.Core.name := "name"
            )
          ),
          ConstructorDef(
            "Bar",
            List("age"),
            ConstructorApp(TpeConstructor1(FOAF.Core.Person, List()),
              FOAF.Core.age := "age"
            )
          )
        )
      }
    }

    'sbfile - {
      parse("""
          |WithNameAge(name, age) => WithAge(age)
          |  foaf:name = name
          |
          |WithAge(age) => foaf:person
          |  foaf:age = age
          |
          |me : WithNameAge("matthew", 40)
          |  foaf:knows = "caroline"
          |""".stripMargin) in Ø evaluatesWithRanges parse_instances_eval(
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
            |""".stripMargin) :_*)
    }

    'instance_reassignment - {
      val (ctxt, res) = parse(
        """a : x
          |a : y
          |a : z""".stripMargin).eval.run(Ø)
      val aRes = ctxt.resolveInst("a")
      assert(aRes == Some(longhandAst.InstanceExp("a", longhandAst.ConstructorApp(longhandAst.TpeConstructor("z"), List()))))
      assert(ctxt.insts("a").length == 3)
    }
  }
}
