package uk.co.turingatemyhamster.shortbol
package  ops

import shorthandAst._
import sugar._

object DNAFormatConversion {
  val XsdString = "xsd" :# "string"

  val EdamFasta = "edam" :# "fasta"
  val EdamGenbank = "edam" :# "genbank"


  lazy val fastaToDNA = RewriteRule ({
    case StringLiteral(style, Some(Datatype(EdamFasta)), _) =>
      val s = style.asString
      val trimmed = if (s startsWith ">") {
        s.substring(s.indexOf('\n') + 1)
      } else s

      StringLiteral(
        StringLiteral.SingleLine(trimmed.replaceAll("[\\s]+", "")),
        None,
        None)
  }: PartialFunction[Literal, Literal])

  lazy val genbankToDNA = RewriteRule ({
    case StringLiteral(style, Some(Datatype(EdamGenbank)), _) =>
      val s = style.asString

      StringLiteral(
        StringLiteral.SingleLine(s.replaceAll("[\\s\\d]+", "")),
        None,
        None)
  } : PartialFunction [Literal, Literal])

}