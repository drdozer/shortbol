package uk.co.turingatemyhamster.shortbol

import fastparse.Parser
import fastparse.core.Result
import utest._

/**
 * Created by chris on 17/07/15.
 */
object ParserTestSuite extends TestSuite{
  val parser = new ShortbolParser()

  def shouldParse[T](txt: String, p: Parser[T], value: T): Unit = {
    p.parse(txt) match {
      case s : Result.Success.Mutable[T] =>
        assert(s.value == value)
    }
  }

  /*scala test for automated testing - random leters ext */

  val tests = TestSuite {

    'LocalName - {
      shouldParse("anAlphaIdentifier", parser.LocalName, LocalName("anAlphaIdentifier"))
      shouldParse("a1234", parser.LocalName, LocalName("a1234"))
    }

  }


}
