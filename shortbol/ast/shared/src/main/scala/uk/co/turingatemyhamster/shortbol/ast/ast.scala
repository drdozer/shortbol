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
  case class BlankLine(blankLine: ast.BlankLine) extends BodyStmt
  case class Comment(comment: ast.Comment) extends BodyStmt
  case class PropertyExp(propertyExp: ast.PropertyExp) extends BodyStmt
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

case class StringLiteral(style: StringLiteral.Style, datatype: Option[Datatype] = None, language: Option[Language] = None) extends Literal

object StringLiteral {
  sealed trait Style extends AstNode {
    def asString: String
  }

  case class SingleLine(asString: String, escaped: Boolean = false) extends Style {
    def isEscaped = escaped || asString.contains("\"")
  }

  case class MultiLine(ss: Seq[String], indent: Int) extends Style {
    def asString = ss mkString "\n"
  }
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

object ConstructorApp {
  def apply[T](cstr: T, bodys: BodyStmt*)(implicit e: T => TpeConstructor): ConstructorApp =
    ConstructorApp(e(cstr), bodys)
}

case class ConstructorDef(id: Identifier,
                            args: Seq[Identifier],
                            cstrApp: ConstructorApp) extends AstNode

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp) extends AstNode

case class Pragma(id: Identifier, values: Seq[ast.ValueExp]) extends AstNode

// the whole thing
case class SBFile(tops: Seq[TopLevel]) extends AstNode

case class SBEvaluatedFile(tops: Seq[TopLevel.InstanceExp]) extends AstNode

case class PropertyExp(property: Identifier, value: PropertyValue) extends AstNode

sealed trait PropertyValue

object PropertyValue {
  case class Literal(value: ast.Literal) extends PropertyValue
  case class Reference(value: ast.Identifier) extends PropertyValue
  case class Nested(value: ast.ConstructorApp) extends PropertyValue
}

object sugar {
  import scala.language.implicitConversions

  implicit def tlAssignment[A](a: A)(implicit e: A => ast.Assignment): TopLevel.Assignment = TopLevel.Assignment(a)
  implicit def tlBlankLine(bl: BlankLine): TopLevel.BlankLine = TopLevel.BlankLine(bl)
  implicit def tlComment(c: Comment): TopLevel.Comment = TopLevel.Comment(c)
  implicit def tlConstructorDef(c: ConstructorDef): TopLevel.ConstructorDef = TopLevel.ConstructorDef(c)

  implicit def idToTpe[I](i: I)(implicit e: I => Identifier): TpeConstructor1 =
    TpeConstructor1(i, Nil)

  implicit class IdentifierToTpe[I](_i: I) {
    def withArgs(ves: ValueExp*)(implicit iE: I => Identifier): TpeConstructor =
      TpeConstructor1(_i, ves)
  }

  implicit def bsBlankLine(bl: BlankLine): BodyStmt.BlankLine = BodyStmt.BlankLine(bl)
  implicit def bsComment(c: Comment): BodyStmt.Comment = BodyStmt.Comment(c)
  implicit def bsPropertyExp[PE](pe: PE)(implicit peE: PE => PropertyExp): BodyStmt.PropertyExp = BodyStmt.PropertyExp(pe)

  implicit def pvLiteral[L](l: L)(implicit lE: L => Literal): PropertyValue = PropertyValue.Literal(l)
  implicit def pvReference[R](r: R)(implicit rE: R => Identifier): PropertyValue = PropertyValue.Reference(r)
  implicit def pvNested(n: ast.ConstructorApp): PropertyValue = PropertyValue.Nested(n)

  implicit def veIdentifier[I](i: I)(implicit e: I => ast.Identifier): ValueExp.Identifier = ValueExp.Identifier(i)
  implicit def veLiteral[L](l: L)(implicit e: L => ast.Literal): ValueExp.Literal = ValueExp.Literal(l)
  def slVal(s: String): ValueExp = veLiteral(slLit(s))

  implicit class IV[I](val _i: I) extends AnyVal {
    def := [V](v: V): I IvPair V = IvPair(_i, v)
  }

  case class IvPair [I, V](i: I, v: V)

  implicit def toAssignment[I, V](iv: I IvPair V)(implicit iE: I => ast.Identifier, vE: V => ValueExp): ast.Assignment =
    ast.Assignment(iv.i, iv.v)
  implicit def toProperty[I, V](iv: I IvPair V)(implicit iE: I => ast.Identifier, vE: V => PropertyValue): PropertyExp =
    PropertyExp(iv.i, iv.v)

  implicit def strLit[S](s: S)(implicit e: S => StringLiteral.Style): StringLiteral = StringLiteral(s, None, None)
  def slLit(s: String): StringLiteral = StringLiteral(StringLiteral.SingleLine(s, false), None, None)

  implicit def strLN(s: String): LocalName = LocalName(s)
  implicit def strNP(s: String): NSPrefix = NSPrefix(s)
  implicit def intIL(i: Int): IntegerLiteral = IntegerLiteral(i)

  implicit class NSPrefixOps[N](val _pfx: N) extends AnyVal {
    def :# (ln: LocalName)(implicit ne: N => NSPrefix): QName = QName(_pfx, ln)
  }

}