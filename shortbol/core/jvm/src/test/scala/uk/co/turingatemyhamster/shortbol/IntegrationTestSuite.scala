package uk.co.turingatemyhamster.shortbol

import utest._

import uk.co.turingatemyhamster.shortbol.EvalTestSuite._
import ast._

object IntegrationTestSuite extends TestSuite {
  override def tests = TestSuite {
    * - {
      parse("""@import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol/genomic.sbol>
              |p : Promoter""".stripMargin) in Fixture.configuredContext evaluatesTo
        parse_instances(
          """p : ComponentDefinition
            |  type = <SBOL:DNA>
            |  role = <SBOL:Promoter>""".stripMargin) in ⊥
    }
  }
}
