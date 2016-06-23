package uk.co.turingatemyhamster
package shortbol

import ast._
import ast.{Pragma => _}
import ast.sugar._
import pragma._
import pragma.{MetaPragma => _}

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
        val startCtxt = Import(Resolver.fromValues(
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

  }

}
