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
object FindAllTestSuite extends TestSuite {
  override def tests = TestSuite {
    val f = parse(
      """@prefix foo <http://some.com/stuff#>
        |foo:me : foaf:person""".stripMargin
    )

    'hit - {
      val s = FindAll[String].apply("hi")
      assert(s == List("hi"))
    }

    'miss - {
      val s = FindAll[String].apply(4)
      assert(s == Nil)
    }

    'pair - {
      * - {
        val p = FindAll[String].apply((4, 2))
        assert(p == Nil)
      }

      * - {
        val p = FindAll[String].apply(("left", 2))
        assert(p == List("left"))
      }

      * - {
        val p = FindAll[String].apply((4, "right"))
        assert(p == List("right"))
      }

      * - {
        val p = FindAll[String].apply(("left", "right"))
        assert(p == List("left", "right"))
      }
    }

    'list - {
      * - {
        val s = FindAll[String].apply(List(1,2,3,4))
        assert(s == List())
      }
    }

    'list - {
      * - {
        val s = FindAll[String].apply(List(1,"two",3,"four"))
        assert(s == List("two", "four"))
      }

        * - {
          val s = FindAll[String].apply(List(List("a", 'b),"two",List('c, 'd, "e"),"four"))
          assert(s == List("two", "four"))
        }
    }

    'identifiers - {
      val is = FindAll[Identifier].apply(f)

      assert(is == List[Identifier]("prefix", "foo", Url("http://some.com/stuff#"), ("foo":#"me"), ("foaf":#"person")))
    }

    'string - {
      val ss = FindAll[String].apply(f)

      assert(ss == List("prefix", "foo", "http://some.com/stuff#", "foo", "me", "foaf", "person"))
    }
  }
}
