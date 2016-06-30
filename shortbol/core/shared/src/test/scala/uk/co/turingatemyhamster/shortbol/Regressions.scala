package uk.co.turingatemyhamster.shortbol

import java.io.StringWriter

import ast._
import uk.co.turingatemyhamster.shortbol.ops.{PrettyPrinter, ShortbolParser}
import utest._

/**
  *
  *
  * @author Matthew Pocock
  */
object Regressions extends TestSuite {
  override def tests = TestSuite {
    'evalpp - {
      val txt =
        """df:me : df:you
          |  df:name = "matthew"""".stripMargin

      val got = ShortbolParser.SBFile.parse(txt).get.value

      val expected = SBFile(List(
        TopLevel.InstanceExp(
          InstanceExp(QName(NSPrefix("df"),LocalName("me")),
            ConstructorApp(
              TpeConstructor1(QName(NSPrefix("df"),LocalName("you")),List()),
              List(BodyStmt.Assignment(
                Assignment(
                  QName(NSPrefix("df"),LocalName("name")),
                  ValueExp.Literal(StringLiteral(StringLiteral.SingleLine("matthew",false),None,None))
                )
              ))
            )
          )
        )
      ))

      val writer = new StringWriter()
      PrettyPrinter(writer)(got)
      val ppTxt = writer.toString

      * - assert(got == expected)
      * - assert(ppTxt.trim() == txt.trim())

    }
  }
}
