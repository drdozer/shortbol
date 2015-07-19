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

  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
    p.parse(txt) match {
      case s : Result.Success.Mutable[T] =>
        assert(false)

      case f: Result.Failure.Mutable =>
        assert(true)
    }
  }

  /*scala test for automated testing - random leters ext */

  val tests = TestSuite {

    'LocalName - {
      shouldParse("anAlphaIdentifier", parser.LocalName, LocalName("anAlphaIdentifier"))
      shouldParse("a1234", parser.LocalName, LocalName("a1234"))
      shouldParse("_ajfh13", parser.LocalName,LocalName("_ajfh13"))
      shouldParse("a1234.abc-c",parser.LocalName,LocalName("a1234.abc-c"))
      shouldNotParse("1abc",parser.LocalName)
      shouldNotParse(".abc1",parser.LocalName)
      shouldNotParse("-abc1",parser.LocalName)
    }

    'NSPrefix - {
      shouldParse("anAlphaIdentifier", parser.NSPrefix, NSPrefix("anAlphaIdentifier"))
      shouldParse("a1234", parser.NSPrefix, NSPrefix("a1234"))
      shouldParse("_ajfh13", parser.NSPrefix,NSPrefix("_ajfh13"))
      shouldParse("a1234.abc-c",parser.NSPrefix,NSPrefix("a1234.abc-c"))
      shouldNotParse("1abc",parser.NSPrefix)
      shouldNotParse(".abc1",parser.NSPrefix)
      shouldNotParse("-abc1",parser.NSPrefix)
    }

    'QName - {

      shouldParse("a123:b234",parser.QName,QName(NSPrefix("a123"),LocalName("b234")))
      shouldParse("_a123.2:b234-5",parser.QName,QName(NSPrefix("_a123.2"),LocalName("b234-5")))
      shouldNotParse("._a123.2:1b234-5",parser.QName)
      shouldNotParse("abc : cba ",parser.QName)
      shouldNotParse("abc :cba ",parser.QName)
      shouldNotParse("abc: cba ",parser.QName)
      
    }

  }


}
