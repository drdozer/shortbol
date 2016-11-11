package uk.co.turingatemyhamster
package shortbol

import shorthandAst._
import shorthandAst.{Pragma => _}
import shorthandAst.sugar._
import pragma._
import pragma.{PragmaPragma => _}
import ops.Eval._
import uk.co.turingatemyhamster.shortbol.ops.{LogMessage, ShortbolParser}
import ShortbolParser.POps
import utest._

object PragmaTestSuite extends TestSuite {

  val tests = TestSuite {
    import EvalTestSuite.{parse, parseLB, parse_instances, parse_instances_eval, parse_constructorDef, Ø, TestOps}

    'pragma - {
      (Fixture.configuredContext.prgms,
        Fixture.configuredContext.logms)
    }

    'import - {
      * - {
        val pragmaP = PragmaPragma()
        val importPID = ValueExp.Identifier(ImportPragma.ID)
        val is = Fixture.configuredContext.prgms(pragmaP.ID) filter (_.values.head == importPID)
        assert(is.nonEmpty)
      }

      'importNothing - {
        val startCtxt = Ø

        parse(
          """@import <http://xmlns.com/foaf/0.1/>
            |me : WithNameAge("matthew", 40)
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt evaluatesWithRanges parse_instances_eval(
          """me : WithNameAge
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt.withInstances(
          parse_instances(
            """me : WithNameAge
              |  foaf:knows = "caroline"
              |""".stripMargin) :_*).withPragmas(
          Pragma("import", List(Url("http://xmlns.com/foaf/0.1/")))
        )
      }

      'importDefsApplyNothing - {
        val bootstrapPragmas = PragmaPragma(
          ImportPragma(Resolver.fromValues(
            Url("http://xmlns.com/foaf/0.1/") -> parse(
              """WithNameAge(name, age) => WithAge(age)
                |  foaf:name = name
                |
                |WithAge(age) => foaf:person
                |  foaf:age = age
                |""".stripMargin
            )
          )))
        val bootstrap = for {
          _ <- bootstrapPragmas.register(Pragma("pragma", List("pragma")))
          _ <- ShortbolParser.SBFile.withPositions("_bootstrap_", bootstrapPragmas.bootstrap).get.value.eval
        } yield ()
        val startCtxt = bootstrap.exec(Ø)

        parse(
          """@import <http://xmlns.com/foaf/0.1/>
            |""".stripMargin) in startCtxt evaluatesWithRanges parse_instances_eval(
          """""".stripMargin) in startCtxt.withConstructors (
          parse_constructorDef(
            """WithNameAge(name, age) => WithAge(age)
              |  foaf:name = name
              |""".stripMargin),
          parse_constructorDef(
            """WithAge(age) => foaf:person
              |  foaf:age = age
              |""".stripMargin)
        ).withPragmas(
          Pragma("import", List(Url("http://xmlns.com/foaf/0.1/")))
        )
      }

      'importDefsAndApply - {
        val bootstrapPragmas = PragmaPragma(
          ImportPragma(Resolver.fromValues(
            Url("http://xmlns.com/foaf/0.1/") -> parse(
              """WithNameAge(name, age) => WithAge(age)
                |  foaf:name = name
                |
                |WithAge(age) => foaf:person
                |  foaf:age = age
                |""".stripMargin
            )
          )))
        val bootstrap = for {
          _ <- bootstrapPragmas.register(Pragma("pragma", List("pragma")))
          _ <- ShortbolParser.SBFile.withPositions("_bootstrap_", bootstrapPragmas.bootstrap).get.value.eval
        } yield ()
        val startCtxt = bootstrap.exec(Ø)

        parse(
          """@import <http://xmlns.com/foaf/0.1/>
            |me : WithNameAge("matthew", 40)
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt evaluatesWithRanges parse_instances_eval(
          """me : foaf:person
            |  foaf:age = 40
            |  foaf:name = "matthew"
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt.withConstructors (
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
                    |""".stripMargin) :_*).withPragmas(
          Pragma("import", List(Url("http://xmlns.com/foaf/0.1/")))
        )
      }
    }


    'defaultPrefix - {
      * - {
        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in Ø evaluatesWithRanges parse_instances_eval(
          """me : foaf:person"""
        ) in Ø.withInstances(parse_instances(
          """me : foaf:person"""
        ) :_*).withPragmas(
          Pragma("defaultPrefix", List("pfx"))
        )
      }

      * - {
        val startCtxt = DefaultPrefixPragma.apply.register(Pragma("pragma", List("defaultPrefix", "prefixName"))).exec(Ø)

        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in startCtxt evaluatesWithRanges parse_instances_eval(
          """pfx:me : foaf:person"""
        ) in startCtxt.withInstances(parse_instances(
          """pfx:me : foaf:person"""
        ) :_*).withPragmas(
          Pragma("defaultPrefix", List("pfx"))
        )
      }

      * - {
        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in Fixture.configuredContext evaluatesWithRanges parse_instances_eval(
          """pfx:me : foaf:person"""
        ) in Fixture.configuredContext.withInstances(parse_instances(
          """pfx:me : foaf:person"""
        ) :_*).withPragmas(
          Pragma("defaultPrefix", List("pfx"))
        )
      }

      * - {
        parse(
          """@defaultPrefix pfx
            |
            |foo(x) => bar
            |  y = x""".stripMargin) in
          Fixture.configuredContext evaluatesWithRanges
          longhandAst.SBFile(List.empty) in
          Fixture.configuredContext.withConstructors(
            parse_constructorDef(
              """pfx:foo(pfx:x) => pfx:bar
                |  pfx:y = pfx:x""".stripMargin)
          ).withPragmas(Pragma("defaultPrefix", "pfx"::Nil))
      }
    }

    'prefix - {
      * - {
        parse("@prefix foo <http://some.com/stuff#>") in
          Fixture.configuredContext evaluatesWithRanges longhandAst.SBFile(List.empty) in
          Fixture.configuredContext.withPragmas(Pragma("prefix", List("foo", Url("http://some.com/stuff#"))))
      }

      * - {
        val c = parse(
          "@prefix foo <http://some.com/stuff#>"
        ).eval.exec(Fixture.configuredContext)
        (c.logms, c.prgms)
      }

      * - {
        parse("foo:me : foaf:person") in Fixture.configuredContext evaluatesWithRanges parse_instances_eval(
          "foo:me : foaf:person"
        ) in Fixture.configuredContext.withInstances(parse_instances(
          "foo:me : foaf:person"
        ) :_*)
      }

      * - {
        val c = parse(
          "foo:me : foaf:person"
        ).eval.exec(Fixture.configuredContext)
        (c.logms, c.prgms)
      }


      * - {
        parse(
          """@prefix foo <http://some.com/stuff#>
            |foo:me : foaf:person""".stripMargin) in Fixture.configuredContext evaluatesWithRanges
          parse_instances_eval(
          "foo:me : foaf:person"
        ) in
          Fixture.configuredContext.withInstances(
          parse_instances(
            "foo:me : foaf:person"
          ) :_*).withPragmas(
          Pragma("prefix", List("foo", Url("http://some.com/stuff#")))
        )
      }

      val msgPfx = "No prefix binding for "

      * - {
        val log = parse(
          """foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _, _) if msg startsWith msgPfx => msg.substring(msgPfx.length) }

        val foo = pb filter (_.split(" ").head contains "foo")
        val foaf = pb filter (_.split(" ").head contains "foaf")

        assert(foo != Seq.empty)
        assert(foaf != Seq.empty)
        pb
      }

      * - {
        val log = parse(
          """@prefix foo <http://some.com/stuff#>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _, _) if msg startsWith msgPfx => msg.substring(msgPfx.length) }

        val foo = pb filter (_.split(" ").head contains "foo")
        val foaf = pb filter (_.split(" ").head contains "foaf")

        assert(foo == Seq.empty)
        assert(foaf != Seq.empty)
        log.map(_.pretty).mkString("\n")
        pb map (_.split(" ").head)
      }

      * - {
        val log = parse(
          """@prefix foaf <http://xmlns.com/foaf/0.1/>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _, _) if msg startsWith msgPfx => msg.substring(msgPfx.length) }

        val foo = pb filter (_.split(" ").head contains "foo")
        val foaf = pb filter (_.split(" ").head contains "foaf")

        assert(foo != Seq.empty)
        assert(foaf == Seq.empty)
        log.map(_.pretty).mkString("\n")
        pb map (_.split(" ").head)
      }

      * - {
        val log = parse(
          """@prefix foo <http://some.com/stuff#>
            |@prefix foaf <http://xmlns.com/foaf/0.1/>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _, _) if msg startsWith msgPfx => msg.substring(msgPfx.length) }

        val foo = pb filter (_.split(" ").head contains "foo")
        val foaf = pb filter (_.split(" ").head contains "foaf")

        assert(foo == Seq.empty)
        assert(foaf == Seq.empty)
        pb map (_.split(" ").head)
      }

    }
  }

}
