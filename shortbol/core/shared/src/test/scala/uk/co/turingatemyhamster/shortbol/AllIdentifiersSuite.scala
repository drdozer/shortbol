package uk.co.turingatemyhamster
package shortbol

import ast._
import ast.sugar._
import ops._
import utest._
import EvalTestSuite.parse
import utest.framework.{Test, Tree}

/**
  * Created by nmrp3 on 24/06/16.
  */
object AllIdentifiersSuite extends TestSuite {
  override def tests = TestSuite {
    val f = parse(
      """@prefix foo <http://some.com/stuff#>
        |foo:me : foaf:person""".stripMargin
    )

    'identifiers - {
      val is = AllIdentifiers[SBFile].apply(f)

      assert(is == List[Identifier]("prefix", "foo", Url("http://some.com/stuff#"), ("foo":#"me"), ("foaf":#"person")))
    }
  }
}
