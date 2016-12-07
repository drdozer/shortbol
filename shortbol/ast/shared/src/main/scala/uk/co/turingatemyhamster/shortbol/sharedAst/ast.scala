package uk.co.turingatemyhamster.shortbol
package sharedAst


case class Pos(offset: Int, line: Int = -1, column: Int = -1)
{
  def pretty = s"line: $line, col: $column"
}

case class Region(startsAt: Pos, endsAt: Pos, in: Identifier)
{
  def pretty = s"${startsAt.pretty} to ${endsAt.pretty} in $in"
}

trait AstNode {
  var region: Region = _
}

// identifiers
// but consider: https://www.w3.org/TR/REC-xml-names/#ns-qualnames
sealed trait Identifier extends AstNode
case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String) extends AstNode
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier


// literals
sealed trait Literal extends sharedAst.AstNode

case class StringLiteral(style: StringLiteral.Style, datatype: Option[Datatype] = None, language: Option[Language] = None) extends Literal

case class Datatype(tpe: sharedAst.Identifier) extends sharedAst.AstNode
case class Language(tag: String) extends sharedAst.AstNode

object StringLiteral {
  sealed trait Style extends sharedAst.AstNode {
    def asString: String
  }

  case class SingleLine(asString: String, escaped: Boolean = false) extends Style {
    def isEscaped = escaped || asString.contains("\"")
  }

  case class MultiLine(ss: List[String], indent: Int) extends Style {
    def asString = ss mkString "\n"
  }
}

case class IntegerLiteral(i: Int) extends Literal


object sugar {
  import scala.language.implicitConversions

  implicit def strLN(s: String): LocalName = LocalName(s)
  implicit def strNP(s: String): NSPrefix = NSPrefix(s)

  implicit class NSPrefixOps[N](val _pfx: N) extends AnyVal {
    def :# (ln: LocalName)(implicit ne: N => NSPrefix): QName = QName(_pfx, ln)
  }

  implicit def strLit[S](s: S)(implicit e: S => StringLiteral.Style): StringLiteral = StringLiteral(s, None, None)
  def slLit(s: String): StringLiteral = StringLiteral(StringLiteral.SingleLine(s, false), None, None)
  implicit def intIL(i: Int): IntegerLiteral = IntegerLiteral(i)



  implicit class IV[I](val _i: I) extends AnyVal {
    def := [V](v: V): I IvPair V = IvPair(_i, v)
  }

  case class IvPair [I, V](i: I, v: V)

}
