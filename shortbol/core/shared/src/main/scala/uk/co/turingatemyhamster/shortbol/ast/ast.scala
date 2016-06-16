package uk.co.turingatemyhamster.shortbol
package ast

sealed trait TopLevel
sealed trait BodyStmt
sealed trait ValueExp
sealed trait TpeConstructor
sealed trait Identifier extends ValueExp
sealed trait Literal extends ValueExp

// top levels
object TopLevel {
  case class Assignment(assignment: ast.Assignment) extends TopLevel
  case class BlankLine(blankLine: ast.BlankLine.type) extends TopLevel
  case class Comment(comment: ast.Comment) extends TopLevel
  case class Import(path: Identifier) extends TopLevel
  case class InstanceExp(instanceExp: ast.InstanceExp) extends TopLevel
  case class ConstructorDef(id: Identifier,
                            args: Seq[LocalName],
                            cstrApp: ConstructorApp) extends TopLevel
}

object BodyStmt {
  case class Assignment(assignment: ast.Assignment) extends BodyStmt
  case class BlankLine(blankLine: ast.BlankLine.type) extends BodyStmt
  case class Comment(comment: ast.Comment) extends BodyStmt
  case class InstanceExp(instanceExp: ast.InstanceExp) extends BodyStmt
  case class ConstructorApp(constructorApp: ast.ConstructorApp) extends BodyStmt
}

// identifiers
case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String)
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier

// literals
case class StringLiteral(s: String, escaped: Boolean = false) extends Literal {
  def isEscaped = escaped || s.contains("\"")
}

case class MultiLineLiteral(ss: Seq[String], indent: Int) extends Literal

case class IntegerLiteral(i: Int) extends Literal

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

// the whole thing
case class SBFile(tops: Seq[TopLevel] = Seq.empty, rdfAbout: Option[Url] = None, source: Option[Url] = None)