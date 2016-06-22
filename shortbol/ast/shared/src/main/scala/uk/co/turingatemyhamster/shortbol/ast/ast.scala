package uk.co.turingatemyhamster.shortbol.ast

import uk.co.turingatemyhamster.shortbol.ast

sealed trait TopLevel
sealed trait BodyStmt
sealed trait ValueExp
sealed trait TpeConstructor
sealed trait Identifier
sealed trait Literal

// top levels
object TopLevel {
  case class Assignment(assignment: ast.Assignment) extends TopLevel
  case class BlankLine(blankLine: ast.BlankLine.type) extends TopLevel
  case class Comment(comment: ast.Comment) extends TopLevel
  case class Import(path: Identifier) extends TopLevel
  case class InstanceExp(instanceExp: ast.InstanceExp) extends TopLevel
  case class ConstructorDef(constructorDef: ast.ConstructorDef) extends TopLevel
}

// body statements
object BodyStmt {
  case class Assignment(assignment: ast.Assignment) extends BodyStmt
  case class BlankLine(blankLine: ast.BlankLine.type) extends BodyStmt
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
case class NSPrefix(pfx: String)
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier

// literals

case class StringLiteral(string: StringLiteral.Style, datatype: Option[Datatype] = None, language: Option[Language] = None) extends Literal

object StringLiteral {
  sealed trait Style

  case class SingleLine(s: String, escaped: Boolean = false) extends Style {
    def isEscaped = escaped || s.contains("\"")
  }

  case class MultiLine(ss: Seq[String], indent: Int) extends Style
}

case class IntegerLiteral(i: Int) extends Literal

case class Datatype(iri: String)
case class Language(tag: String)

// type constructors
case class TpeConstructor1(id: Identifier, args: Seq[ValueExp]) extends TpeConstructor
case object TpeConstructorStar extends TpeConstructor

// expressions
case class Assignment(property: Identifier, value: ValueExp)

case object BlankLine

case class Comment(commentText: String)

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp)

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[BodyStmt])

case class ConstructorDef(id: Identifier,
                            args: Seq[LocalName],
                            cstrApp: ConstructorApp)

// the whole thing
case class SBFile(tops: Seq[TopLevel] = Seq.empty, rdfAbout: Option[Url] = None, source: Option[Url] = None)

object sugar {
  import scala.language.implicitConversions

  implicit def tlAssignment[A](a: A)(implicit e: A => ast.Assignment): TopLevel.Assignment = TopLevel.Assignment(a)
  implicit def tlBlankLine(bl: BlankLine.type): TopLevel.BlankLine = TopLevel.BlankLine(bl)
  implicit def tlComment(c: Comment): TopLevel.Comment = TopLevel.Comment(c)
  implicit def tlConstructorDef(c: ConstructorDef): TopLevel.ConstructorDef = TopLevel.ConstructorDef(c)

  implicit def bsAssignment[A](a: A)(implicit e: A => ast.Assignment): BodyStmt.Assignment = BodyStmt.Assignment(a)
  implicit def bsBlankLine(bl: BlankLine.type): BodyStmt.BlankLine = BodyStmt.BlankLine(bl)
  implicit def bsComment(c: Comment): BodyStmt.Comment = BodyStmt.Comment(c)
  implicit def bsInstanceExp(ie: InstanceExp): BodyStmt.InstanceExp = BodyStmt.InstanceExp(ie)

  implicit def veIdentifier[I](i: I)(implicit e: I => ast.Identifier): ValueExp.Identifier = ValueExp.Identifier(i)
  implicit def veLiteral[L](l: L)(implicit e: L => ast.Literal): ValueExp.Literal = ValueExp.Literal(l)

  implicit def ass[A, B](ab: (A, B))(implicit ai: A => Identifier, bv: B => ValueExp): Assignment = Assignment(ab._1, ab._2)

  implicit def strLit[S](s: S)(implicit e: S => StringLiteral.Style): StringLiteral = StringLiteral(s, None, None)
//  implicit def strL(s: String): StringLiteral.SingleLine = StringLiteral.SingleLine(s)

  implicit def strLN(s: String): LocalName = LocalName(s)
  implicit def strNP(s: String): NSPrefix = NSPrefix(s)
  implicit def intIL(i: Int): IntegerLiteral = IntegerLiteral(i)

  implicit class NSPrefixOps[N](val _pfx: N) extends AnyVal {
    def :# (ln: LocalName)(implicit ne: N => NSPrefix): QName = QName(_pfx, ln)
  }
}