package uk.co.turingatemyhamster
package shortbol

import ast._
import ast.{Pragma => _}
import ast.sugar._
import pragma._
import pragma.{PragmaPragma => _}
import ops.Eval._
import uk.co.turingatemyhamster.shortbol.ops.LogMessage
import utest._

object PragmaTestSuite extends TestSuite {

  val tests = TestSuite {
    import EvalTestSuite.{parse, parse_instances, parse_constructorDef, Ø, TestOps}

    'import - {
      * - {
        val startCtxt = Ø

        parse(
          """@import <http://xmlns.com/foaf/0.1/>
            |me : WithNameAge("matthew", 40)
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt evaluatesTo parse_instances(
          """me : WithNameAge("matthew", 40)
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt.withInstances(
          parse_instances(
            """me : WithNameAge("matthew", 40)
              |  foaf:knows = "caroline"
              |""".stripMargin).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("import", Seq(Url("http://xmlns.com/foaf/0.1/")))
        )
      }

      * - {
        val startCtxt = ImportPragma(Resolver.fromValues(
                  Url("http://xmlns.com/foaf/0.1/") -> parse(
                    """WithNameAge(name, age) => WithAge(age)
                      |  foaf:name = name
                      |
                      |WithAge(age) => foaf:person
                      |  foaf:age = age
                      |""".stripMargin
                  )
                )).register(Pragma("pragma", Seq("import", "url"))).exec(Ø)

        parse(
          """@import <http://xmlns.com/foaf/0.1/>
            |me : WithNameAge("matthew", 40)
            |  foaf:knows = "caroline"
            |""".stripMargin) in startCtxt evaluatesTo parse_instances(
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
                    |""".stripMargin).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("import", Seq(Url("http://xmlns.com/foaf/0.1/")))
        )
      }
    }


    'defaultPrefix - {
      * - {
        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in Ø evaluatesTo parse_instances(
          """me : foaf:person"""
        ) in Ø.withInstances(parse_instances(
          """me : foaf:person"""
        ).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("defaultPrefix", Seq("pfx"))
        )
      }

      * - {
        val startCtxt = DefaultPrefixPragma.apply.register(Pragma("pragma", Seq("defaultPrefix", "prefixName"))).exec(Ø)

        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in startCtxt evaluatesTo parse_instances(
          """pfx:me : foaf:person"""
        ) in startCtxt.withInstances(parse_instances(
          """pfx:me : foaf:person"""
        ).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("defaultPrefix", Seq("pfx"))
        )
      }

      * - {
        parse(
          """@defaultPrefix pfx
            |
            |me : foaf:person""".stripMargin) in Fixture.configuredContext evaluatesTo parse_instances(
          """pfx:me : foaf:person"""
        ) in Fixture.configuredContext.withInstances(parse_instances(
          """pfx:me : foaf:person"""
        ).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("defaultPrefix", Seq("pfx"))
        )
      }
    }

    'prefix - {
      * - {
        parse("@prefix foo <http://some.com/stuff#>") in
          Fixture.configuredContext evaluatesTo Seq.empty[TopLevel.InstanceExp] in
          Fixture.configuredContext.withPragmas(Pragma("prefix", Seq("foo", Url("http://some.com/stuff#"))))
      }

      * - {
        val c = parse(
          "@prefix foo <http://some.com/stuff#>"
        ).eval.exec(Fixture.configuredContext)
        (c.logms, c.prgms)
      }

      * - {
        parse("foo:me : foaf:person") in Fixture.configuredContext evaluatesTo parse_instances(
          "foo:me : foaf:person"
        ) in Fixture.configuredContext.withInstances(parse_instances(
          "foo:me : foaf:person"
        ).map { i => i.instanceExp} :_*)
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
            |foo:me : foaf:person""".stripMargin) in Fixture.configuredContext evaluatesTo parse_instances(
          "foo:me : foaf:person"
        ) in Fixture.configuredContext.withInstances(parse_instances(
          "foo:me : foaf:person"
        ).map { i => i.instanceExp} :_*).withPragmas(
          Pragma("prefix", Seq("foo", Url("http://some.com/stuff#")))
        )
      }

      * - {
        val log = parse(
          """foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _) if msg startsWith "No prefix binding for" => msg }

        val foo = pb filter (_ contains "foo")
        val foaf = pb filter (_ contains "foaf")

        assert(foo.nonEmpty)
        assert(foaf.nonEmpty)
      }

      * - {
        val log = parse(
          """@prefix foo <http://some.com/stuff#>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _) if msg startsWith "No prefix binding for" => msg }

        val foo = pb filter (_ contains "foo")
        val foaf = pb filter (_ contains "foaf")

        assert(foo.isEmpty)
        assert(foaf.nonEmpty)
      }

      * - {
        val log = parse(
          """@prefix foaf <http://xmlns.com/foaf/0.1/>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _) if msg startsWith "No prefix binding for" => msg }

        val foo = pb filter (_ contains "foo")
        val foaf = pb filter (_ contains "foaf")

        assert(foo.nonEmpty)
        assert(foaf.isEmpty)
      }

      * - {
        val log = parse(
          """@prefix foo <http://some.com/stuff#>
            |@prefix foaf <http://xmlns.com/foaf/0.1/>
            |foo:me : foaf:person
          """.stripMargin).eval.exec(Fixture.configuredContext).logms

        val pb = log.collect { case LogMessage(msg, _, _) if msg startsWith "No prefix binding for" => msg }

        val foo = pb filter (_ contains "foo")
        val foaf = pb filter (_ contains "foaf")

        assert(foo.isEmpty)
        assert(foaf.isEmpty)
      }

    }
  }

}
