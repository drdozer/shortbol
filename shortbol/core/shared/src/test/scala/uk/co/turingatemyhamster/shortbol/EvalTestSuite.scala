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
  def parse(shortbol: String): SBFile =
    ShortbolParser.SBFile.parse(shortbol) match {
      case s : Success[SBFile] =>
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
    def evaluatesTo[U](u: U)(implicit e: EvalEval.Aux[T, U]) = (new InContext(_t, Ø)).evaluatesTo(u)(e)
  }

  class InContext[T](t: T, c0: EvalContext) {
    def evaluatesTo[U](expectedResult: U)(implicit eval: EvalEval.Aux[T, U]) = new Object {
      def in(expectedContext: EvalContext): Unit = {
        assert(eval != null)
        val (observedContext, observedResult) = eval(t).run(c0)

        if(expectedContext != ⊥) assert(observedContext == expectedContext)

        assert(observedResult == expectedResult)
      }
    }
  }

  val tests = TestSuite {

    'blankline - {
      * - { BlankLine evaluatesTo BlankLine in Ø }
      * - { (BlankLine : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) in Ø }
      * - { (BlankLine : BodyStmt) evaluatesTo (BlankLine : BodyStmt) in Ø }
    }

    'comment - {
      * - { Comment("a comment") evaluatesTo Comment("a comment") in Ø }
      * - { (Comment("a comment") : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) in Ø }
      * - { (Comment("a comment") : BodyStmt) evaluatesTo (Comment("a comment") : BodyStmt) in Ø }
    }

    'stringLiteral - {
      * - { StringLiteral("abc", false) evaluatesTo StringLiteral("abc", false) in Ø }
    }

    'multiLineLiteral - {
      * - { MultiLineLiteral(Seq("abc", "def"), 4) evaluatesTo MultiLineLiteral(Seq("abc", "def"), 4) in Ø }
    }

    'integerLiteral - {
      * - { IntegerLiteral(42) evaluatesTo IntegerLiteral(42) in Ø }
    }

    'literal - {
      * - { (StringLiteral("abc", false) : Literal) evaluatesTo (StringLiteral("abc", false) : Literal) in Ø }
      * - { (MultiLineLiteral(Seq("abc", "def"), 4) : Literal) evaluatesTo (MultiLineLiteral(Seq("abc", "def"), 4) : Literal) in Ø }
      * - { (IntegerLiteral(42) : Literal) evaluatesTo (IntegerLiteral(42) : Literal) in Ø }
    }

    'localName - {
      * - { LocalName("a") evaluatesTo ("a" : Identifier) in Ø }
      * - { LocalName("a") in Ø.withAssignments("a" -> "x") evaluatesTo ("x" : Identifier) in Ø.withAssignments ("a" -> "x") }
      * - { LocalName("a") in Ø.withAssignments("a" -> Url("x")) evaluatesTo (Url("x") : Identifier) in Ø.withAssignments ("a" -> Url("x")) }
      * - { LocalName("a") in Ø.withAssignments("a" -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) in Ø.withAssignments ("a" -> QName("foo", "bar")) }
      * - { LocalName("a") in Ø.withAssignments(QName("foo", "a") -> "x") evaluatesTo ("x" : Identifier) in Ø.withAssignments (QName("foo", "a") -> "x") }
      * - { LocalName("a") in Ø.withAssignments("a" -> 42) evaluatesTo ("a" : Identifier) in Ø.withAssignments ("a" -> 42) }
    }

    'url - {
      * - { Url("a") evaluatesTo (Url("a") : Identifier) in Ø }
      * - { Url("a") in Ø.withAssignments(Url("a") -> "x") evaluatesTo ("x" : Identifier) in Ø.withAssignments (Url("a") -> "x") }
      * - { Url("a") in Ø.withAssignments(Url("a") -> Url("x")) evaluatesTo (Url("x") : Identifier) in Ø.withAssignments (Url("a") -> Url("x")) }
      * - { Url("a") in Ø.withAssignments(Url("a") -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) in Ø.withAssignments (Url("a") -> QName("foo", "bar")) }
    }

    'qname - {
      * - { QName("pfx", "ln") evaluatesTo (QName("pfx", "ln") : Identifier) in Ø }
      * - { QName("pfx", "ln") in Ø.withAssignments(QName("pfx", "ln") -> "x") evaluatesTo ("x" : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> "x") }
      * - { QName("pfx", "ln") in Ø.withAssignments(QName("pfx", "ln") -> Url("x")) evaluatesTo (Url("x") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> Url("x")) }
      * - { QName("pfx", "ln") in Ø.withAssignments(QName("pfx", "ln") -> QName("foo", "bar")) evaluatesTo (QName("foo", "bar") : Identifier) in Ø.withAssignments (QName("pfx", "ln") -> QName("foo", "bar")) }
    }

    'valueExp - {
      * - { (StringLiteral("abc", false) : ValueExp) evaluatesTo (StringLiteral("abc", false) : ValueExp) in Ø }
      * - { (LocalName("a") : ValueExp) in Ø.withAssignments("a" -> "x") evaluatesTo ("x" : ValueExp) in Ø.withAssignments ("a" -> "x") }
      * - { (LocalName("a") : ValueExp) in Ø.withAssignments("a" -> 42) evaluatesTo (42 : ValueExp) in Ø.withAssignments ("a" -> 42) }
      * - { (Url("a") : ValueExp) evaluatesTo (Url("a") : ValueExp) in Ø }
      * - { (QName("pfx", "ln") : ValueExp) evaluatesTo (QName("pfx", "ln") : ValueExp) in Ø }
      * - { (StringLiteral("abc", false) : ValueExp) evaluatesTo (StringLiteral("abc", false) : ValueExp) in Ø }
      * - { (MultiLineLiteral(Seq("abc", "def"), 4) : ValueExp) evaluatesTo (MultiLineLiteral(Seq("abc", "def"), 4) : ValueExp) in Ø }
      * - { (42 : ValueExp) evaluatesTo (42 : ValueExp) in Ø }
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
        ) in Ø
      }

      * - {
        Seq[ValueExp](
          StringLiteral("abc", false),
          LocalName("a"),
          42
        ) in Ø.withAssignments(
          "a" -> "x"
          ) evaluatesTo Seq[ValueExp](
          StringLiteral("abc", false),
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

        * - { ("a" -> "b": Assignment) in Ø.withAssignments("a" -> "x") evaluatesTo ("x" -> "b": Assignment) in Ø.withAssignments ("a" -> "x") }

        * - { ("a" -> "b": Assignment) in Ø.withAssignments("b" -> "y") evaluatesTo ("a" -> "y": Assignment) in Ø.withAssignments ("b" -> "y") }
      }

      'bodyStmt - {
        * - { ("a" -> "b" : BodyStmt) evaluatesTo ("a" -> "b" : BodyStmt) in Ø }

        * - { ("a" -> "b" : BodyStmt) in Ø.withAssignments("a" -> "x") evaluatesTo ("x" -> "b" : BodyStmt) in Ø.withAssignments ("a" -> "x") }

        * - { ("a" -> "b" : BodyStmt) in Ø.withAssignments("b" -> "y") evaluatesTo ("a" -> "y" : BodyStmt) in Ø.withAssignments ("b" -> "y") }
      }

      'topLevel - {
        * - { ("a" -> "b" : TopLevel) evaluatesTo (None : Option[TopLevel.InstanceExp]) in Ø.withAssignments ("a" -> "b") }

        * - { ("a" -> "b" : TopLevel) in Ø.withAssignments("a" -> "x") evaluatesTo (None : Option[TopLevel.InstanceExp]) in Ø.withAssignments ("a" -> "x", "x" -> "b") }

        * - { ("a" -> "b" : TopLevel) in Ø.withAssignments("b" -> "y") evaluatesTo (None : Option[TopLevel.InstanceExp]) in Ø.withAssignments ("b" -> "y", "a" -> "y") }
      }
    }

    'assignments - {
      * - {
        Seq[TopLevel](
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None) in Ø.withAssignments (
          "b" -> "c",
          "a" -> "c")
      }

      * - {
        Seq[TopLevel](
          "c" -> "d",
          "b" -> "c",
          "a" -> "b") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None, None) in Ø.withAssignments (
          "c" -> "d",
          "b" -> "d",
          "a" -> "d")
      }

      * - {
        Seq[TopLevel](
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "bx") evaluatesTo Seq[Option[TopLevel.InstanceExp]](None, None, None) in Ø.withAssignments (
          "bx" -> "cx",
          "cx" -> "dx",
          "ax" -> "dx")
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
            StringLiteral("abc", false),
            LocalName("a"),
            42)
          ) evaluatesTo (
            (TpeConstructor1("X", Seq(
              StringLiteral("abc", false),
              LocalName("a"),
              42)) : TpeConstructor) ->
            Seq[BodyStmt]()
            ) in Ø
        }
        * - {
          TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("a"),
            42)
          ) in Ø.withAssignments (
            "a" -> "x"
            ) evaluatesTo TpeConstructor1("X", Seq(
            StringLiteral("abc", false),
            LocalName("x"),
            42)
            ) in Ø.withAssignments (
            "a" -> "x"
            )
        }
      }

      'rename_and_expand - {
        TpeConstructor1("Foo", Seq(
          StringLiteral("abc", false),
          LocalName("a"),
          42)
        ) in Ø.withAssignments (
          "a" -> "x",
          "Foo" -> "Bar"
          ) evaluatesTo (
          (TpeConstructor1("Foo", Seq(
          StringLiteral("abc", false),
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
      TpeConstructorStar evaluatesTo (TpeConstructorStar, Seq.empty[BodyStmt]) in Ø
    }

    'tpeConstructor - {
      * - { (TpeConstructorStar : TpeConstructor) evaluatesTo ((TpeConstructorStar : TpeConstructor, Seq.empty[BodyStmt])) in Ø }

      * - {
        (TpeConstructor1("X", Seq(
          StringLiteral("abc", false),
          LocalName("a"),
          42)
        ) : TpeConstructor) in Ø.withAssignments(
          "a" -> "x",
          "X" -> "Y"
          ) evaluatesTo (TpeConstructor1("Y", Seq(
          StringLiteral("abc", false),
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
        ) evaluatesTo (None: Option[TopLevel.InstanceExp]) in Ø.withConstructors (
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
        ) evaluatesTo (None: Option[TopLevel.InstanceExp]) in Ø.withConstructors (
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
          |  <foaf:name> = name
          |
          |WithAge(age) => <foaf:person>
          |  <foaf:age> = age
          |
          |me : WithNameAge("matthew", 40)
          |  <foaf:knows> = "caroline"
          |""".stripMargin) in Ø evaluatesTo parse_instances(
        """me : <foaf:person>
          |  <foaf:age> = 40
          |  <foaf:name> = "matthew"
          |  <foaf:knows> = "caroline"
          |""".stripMargin) in Ø.withConstructors (
        parse_constructorDef(
          """WithNameAge(name, age) => WithAge(age)
            |  <foaf:name> = name
            |""".stripMargin),
        parse_constructorDef(
          """WithAge(age) => <foaf:person>
            |  <foaf:age> = age
            |""".stripMargin)
      ).withInstances(
        parse_instances(
          """me : <foaf:person>
            |  <foaf:age> = 40
            |  <foaf:name> = "matthew"
            |  <foaf:knows> = "caroline"
            |""".stripMargin).map { i => i.instanceExp} :_*)
    }

    'import - {
      val startCtxt = Ø.withResolver(Resolver.fromValues(
        Url("http://xmlns.com/foaf/0.1/") -> parse(
          """WithNameAge(name, age) => WithAge(age)
            |  <foaf:name> = name
            |
            |WithAge(age) => <foaf:person>
            |  <foaf:age> = age
            |""".stripMargin
        )
      ))

      parse(
        """import <http://xmlns.com/foaf/0.1/>
          |me : WithNameAge("matthew", 40)
          |  <foaf:knows> = "caroline"
          |""".stripMargin) in startCtxt evaluatesTo parse_instances(
        """me : <foaf:person>
          |  <foaf:age> = 40
          |  <foaf:name> = "matthew"
          |  <foaf:knows> = "caroline"
          |""".stripMargin) in startCtxt.withConstructors (
        parse_constructorDef(
          """WithNameAge(name, age) => WithAge(age)
            |  <foaf:name> = name
            |""".stripMargin),
        parse_constructorDef(
          """WithAge(age) => <foaf:person>
            |  <foaf:age> = age
            |""".stripMargin)
      ).withInstances(
              parse_instances(
                """me : <foaf:person>
                  |  <foaf:age> = 40
                  |  <foaf:name> = "matthew"
                  |  <foaf:knows> = "caroline"
                  |""".stripMargin).map { i => i.instanceExp} :_*)
    }
  }
}
