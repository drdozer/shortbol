package uk.co.turingatemyhamster.shortbol.ast

import uk.co.turingatemyhamster.shortbol.ast

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

sealed trait TopLevel
sealed trait BodyStmt
sealed trait ValueExp
sealed trait TpeConstructor extends AstNode
sealed trait Identifier extends AstNode
sealed trait Literal extends AstNode

// top levels
object TopLevel {
  case class Assignment(assignment: ast.Assignment) extends TopLevel
  case class BlankLine(blankLine: ast.BlankLine) extends TopLevel
  case class Comment(comment: ast.Comment) extends TopLevel
  case class ConstructorDef(constructorDef: ast.ConstructorDef) extends TopLevel
  case class InstanceExp(instanceExp: ast.InstanceExp) extends TopLevel
  case class Pragma(pragma: ast.Pragma) extends TopLevel
}

// body statements
object BodyStmt {
  case class Assignment(assignment: ast.Assignment) extends BodyStmt
  case class BlankLine(blankLine: ast.BlankLine) extends BodyStmt
  case class Comment(comment: ast.Comment) extends BodyStmt
  case class InstanceExp(instanceExp: ast.InstanceExp) extends BodyStmt
  case class ConstructorApp(constructorApp: ast.ConstructorApp) extends BodyStmt
}

// value expressions
object ValueExp {
  case class Identifier(identifier: ast.Identifier) extends ValueExp
  case class Literal(literal: ast.Literal) extends ValueExp
}

// identifiers
// but consider: https://www.w3.org/TR/REC-xml-names/#ns-qualnames
case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String) extends AstNode
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier

// literals

case class StringLiteral(string: StringLiteral.Style, datatype: Option[Datatype] = None, language: Option[Language] = None) extends Literal

object StringLiteral {
  sealed trait Style extends AstNode

  case class SingleLine(s: String, escaped: Boolean = false) extends Style {
    def isEscaped = escaped || s.contains("\"")
  }

  case class MultiLine(ss: Seq[String], indent: Int) extends Style
}

case class IntegerLiteral(i: Int) extends Literal

case class Datatype(tpe: Identifier) extends AstNode
case class Language(tag: String) extends AstNode

// type constructors
case class TpeConstructor1(id: Identifier, args: Seq[ValueExp]) extends TpeConstructor
case class TpeConstructorStar() extends TpeConstructor

// expressions
case class Assignment(property: Identifier, value: ValueExp) extends AstNode

case class BlankLine() extends AstNode

case class Comment(commentText: String) extends AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[BodyStmt]) extends AstNode

case class ConstructorDef(id: Identifier,
                            args: Seq[Identifier],
                            cstrApp: ConstructorApp) extends AstNode

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp) extends AstNode

case class Pragma(id: Identifier, values: Seq[ast.ValueExp]) extends AstNode

// the whole thing
case class SBFile(tops: Seq[TopLevel]) extends AstNode

case class SBEvaluatedFile(tops: Seq[TopLevel.InstanceExp]) extends AstNode

object sugar {
  import scala.language.implicitConversions

  implicit def tlAssignment[A](a: A)(implicit e: A => ast.Assignment): TopLevel.Assignment = TopLevel.Assignment(a)
  implicit def tlBlankLine(bl: BlankLine): TopLevel.BlankLine = TopLevel.BlankLine(bl)
  implicit def tlComment(c: Comment): TopLevel.Comment = TopLevel.Comment(c)
  implicit def tlConstructorDef(c: ConstructorDef): TopLevel.ConstructorDef = TopLevel.ConstructorDef(c)

  implicit def bsAssignment[A](a: A)(implicit e: A => ast.Assignment): BodyStmt.Assignment = BodyStmt.Assignment(a)
  implicit def bsBlankLine(bl: BlankLine): BodyStmt.BlankLine = BodyStmt.BlankLine(bl)
  implicit def bsComment(c: Comment): BodyStmt.Comment = BodyStmt.Comment(c)
  implicit def bsInstanceExp(ie: InstanceExp): BodyStmt.InstanceExp = BodyStmt.InstanceExp(ie)

  implicit def veIdentifier[I](i: I)(implicit e: I => ast.Identifier): ValueExp.Identifier = ValueExp.Identifier(i)
  implicit def veLiteral[L](l: L)(implicit e: L => ast.Literal): ValueExp.Literal = ValueExp.Literal(l)
  def slVal(s: String): ValueExp = veLiteral(slLit(s))

  implicit def ass[A, B](ab: (A, B))(implicit ai: A => Identifier, bv: B => ValueExp): Assignment = Assignment(ab._1, ab._2)

  implicit def strLit[S](s: S)(implicit e: S => StringLiteral.Style): StringLiteral = StringLiteral(s, None, None)
  def slLit(s: String): StringLiteral = StringLiteral(StringLiteral.SingleLine(s, false), None, None)

  implicit def strLN(s: String): LocalName = LocalName(s)
  implicit def strNP(s: String): NSPrefix = NSPrefix(s)
  implicit def intIL(i: Int): IntegerLiteral = IntegerLiteral(i)

  implicit class NSPrefixOps[N](val _pfx: N) extends AnyVal {
    def :# (ln: LocalName)(implicit ne: N => NSPrefix): QName = QName(_pfx, ln)
  }
}