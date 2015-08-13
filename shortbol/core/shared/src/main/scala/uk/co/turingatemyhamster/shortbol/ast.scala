package uk.co.turingatemyhamster.shortbol

trait Identifier extends ValueExp

case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String)
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier
//case class QuotedIdentifier(quote: String) extends Identifier

trait ValueExp
case class StringLiteral(s: String, multiLine: Boolean = false) extends ValueExp {
  def isMultiLine = multiLine || s.contains("\n") || s.contains("\r")
}

case class IntegerLiteral(i: Int) extends ValueExp

trait BodyStmt

case class Assignment(property: Identifier, value: ValueExp) extends BodyStmt with TopLevel

case class NestedInstance(nested: InstanceExp) extends BodyStmt

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[BodyStmt]) extends BodyStmt

trait TpeConstructor

case class TpeConstructor1(id: Identifier, args: Seq[ValueExp]) extends TpeConstructor
case object TpeConstructorStar extends TpeConstructor

trait TopLevel

case object BlankLine extends TopLevel with BodyStmt

case class Import(path: String) extends TopLevel

case class Comment(commentText: String) extends TopLevel with BodyStmt

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp) extends TopLevel

case class ConstructorDef(id: Identifier,
                          args: Seq[LocalName],
                          cstrApp: ConstructorApp) extends TopLevel
