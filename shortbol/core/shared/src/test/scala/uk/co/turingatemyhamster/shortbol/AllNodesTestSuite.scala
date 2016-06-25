package uk.co.turingatemyhamster.shortbol

import ops.AllNodes
import ast._

import utest._

/**
  * Created by nmrp3 on 25/06/16.
  */
object AllNodesTestSuite extends TestSuite {
  override def tests = TestSuite {
    'localName - {
      assert(AllNodes.in(LocalName("ln")) == Seq(LocalName("ln")))
    }

    'qname - {
      assert(AllNodes.in(QName(NSPrefix("pfx"), LocalName("ln"))) == Seq(
        QName(NSPrefix("pfx"), LocalName("ln")),
        NSPrefix("pfx"),
        LocalName("ln")
      ))
    }

    'url - {
      assert(AllNodes.in(Url("http")) == Seq(Url("http")))
    }

    'identifier - {
      * - {
        val v = AllNodes.in(LocalName("ln") : Identifier)
        assert(v == Seq(LocalName("ln")))
      }
      * - {
        assert(AllNodes.in(QName(NSPrefix("pfx"), LocalName("ln")) : Identifier) == Seq(
          QName(NSPrefix("pfx"), LocalName("ln")),
          NSPrefix("pfx"),
          LocalName("ln")
        ))
        'url - assert(AllNodes.in(Url("http") : Identifier) == Seq(Url("http")))
      }
    }

    'blankline - {
      assert(AllNodes.in(BlankLine()) == Seq(BlankLine()))
    }

    'comment - {
      assert(AllNodes.in(Comment("bob")) == Seq(Comment("bob")))
    }

    'singleLine - {
      assert(AllNodes.in(StringLiteral.SingleLine("s")) == Seq(StringLiteral.SingleLine("s")))
    }

    'multiLine - {
      assert(AllNodes.in(StringLiteral.MultiLine(Seq("I", "am", "me"), 3)) == Seq(StringLiteral.MultiLine(Seq("I", "am", "me"), 3)))
    }
  }
}
