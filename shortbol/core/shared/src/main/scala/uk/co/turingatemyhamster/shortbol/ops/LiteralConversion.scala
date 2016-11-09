package uk.co.turingatemyhamster.shortbol
package  ops

import shorthandAst._
import sugar._


trait LiteralConversion {
  def apply(lit: Literal, requiredType: Identifier): Option[Literal]
}

object LiteralConversion {
  def apply(c: LiteralConversion*): LiteralConversion = new LiteralConversion {
    override def apply(lit: Literal,
                       reqTpe: Identifier) =
      c.foldLeft(None: Option[Literal])((o, c) => if(o.isDefined) o else c(lit, reqTpe))
  }

  def apply(f: PartialFunction[(Literal, Identifier), Literal]): LiteralConversion = new LiteralConversion {
    override def apply(lit: Literal,
                       requiredType: Identifier) = f.lift((lit, requiredType))
  }

//  implicit def conver7342.3
}


object DNAFormatConversion {
  val XsdString = "xsd" :# "string"

  val EdamFasta = "edam" :# "fasta"
  val EdamGenbank = "edam" :# "genbank"


  lazy val fastaToDNA = LiteralConversion ({
    case (StringLiteral(style, Some(Datatype(EdamFasta)), _), XsdString) =>
      val s = style.asString
      val trimmed = if (s startsWith ">") {
        s.substring(s.indexOf('\n') + 1)
      } else s

      StringLiteral(
        StringLiteral.SingleLine(trimmed.replaceAll("[\\s]+", "")),
        None,
        None)
  } : PartialFunction [(Literal, Identifier), Literal])

  lazy val genbankToDNA = LiteralConversion ({
    case (StringLiteral(style, Some(Datatype(EdamGenbank)), _), XsdString) =>
      val s = style.asString

      StringLiteral(
        StringLiteral.SingleLine(s.replaceAll("[\\s\\d]+", "")),
        None,
        None)
  } : PartialFunction [(Literal, Identifier), Literal])

}