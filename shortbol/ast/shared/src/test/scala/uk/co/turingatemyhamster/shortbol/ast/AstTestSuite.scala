package uk.co.turingatemyhamster.shortbol.ast

import utest._
import sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
object AstTestSuite extends TestSuite {

  val tests = TestSuite {

    'intIl - {
      assert((42 : IntegerLiteral) == IntegerLiteral(42))
    }

    'strNP - {
      assert(("bob" : NSPrefix) == NSPrefix("bob"))
    }

    'strLN - {
      assert(("echo" : LocalName) == LocalName("echo"))
      assert(("echo" : Identifier) == LocalName("echo"))
    }

    'nsOps - {
      assert(NSPrefix("bob") :# LocalName("echo") == QName(NSPrefix("bob"), LocalName("echo")))
      assert("bob" :# LocalName("echo") == QName(NSPrefix("bob"), LocalName("echo")))
      assert(NSPrefix("bob") :# "echo" == QName(NSPrefix("bob"), LocalName("echo")))
      assert("bob" :# "echo" == QName(NSPrefix("bob"), LocalName("echo")))
    }

    'valueExp - {
      assert((LocalName("bob") : ValueExp) == ValueExp.Identifier(LocalName("bob")))
      assert(("bob" : ValueExp) == ValueExp.Identifier(LocalName("bob")))
      assert((IntegerLiteral(42): ValueExp) == ValueExp.Literal(IntegerLiteral(42)))
      assert((42: ValueExp) == ValueExp.Literal(IntegerLiteral(42)))
    }

    'ass - {
      assert((LocalName("bob") := IntegerLiteral(42) : Assignment) == Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42))))
      assert(("bob" := IntegerLiteral(42) : Assignment) == Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42))))
      assert((LocalName("bob") := 42 : Assignment) == Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42))))
      assert(("bob" := 42 : Assignment) == Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42))))
    }

    'bsAssignment - {
      assert((LocalName("bob") := IntegerLiteral(42) : BodyStmt.PropertyExp) == BodyStmt.PropertyExp(PropertyExp(LocalName("bob"), PropertyValue.Literal(IntegerLiteral(42)))))
      assert(("bob" := IntegerLiteral(42) : BodyStmt.PropertyExp) == BodyStmt.PropertyExp(PropertyExp(LocalName("bob"), PropertyValue.Literal(IntegerLiteral(42)))))
      assert((LocalName("bob") := 42 : BodyStmt.PropertyExp) == BodyStmt.PropertyExp(PropertyExp(LocalName("bob"), PropertyValue.Literal(IntegerLiteral(42)))))
      assert(("bob" := 42 : BodyStmt.PropertyExp) == BodyStmt.PropertyExp(PropertyExp(LocalName("bob"), PropertyValue.Literal(IntegerLiteral(42)))))
    }

    'tlAssignment - {
      assert((LocalName("bob") := IntegerLiteral(42) : TopLevel.Assignment) == TopLevel.Assignment(Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42)))))
      assert(("bob" := IntegerLiteral(42) : TopLevel.Assignment) == TopLevel.Assignment(Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42)))))
      assert((LocalName("bob") := 42 : TopLevel.Assignment) == TopLevel.Assignment(Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42)))))
      assert(("bob" := 42 : TopLevel.Assignment) == TopLevel.Assignment(Assignment(LocalName("bob"), ValueExp.Literal(IntegerLiteral(42)))))
    }
  }

}
