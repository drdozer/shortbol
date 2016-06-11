package uk.co.turingatemyhamster
package shortbol


sealed trait TopLevel
sealed trait BodyStmt
sealed trait ValueExp
sealed trait TpeConstructor
sealed trait Identifier extends ValueExp
sealed trait Literal extends ValueExp

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


// top levels
case class Assignment(property: Identifier, value: ValueExp) extends TopLevel with BodyStmt

case class TpeConstructor1(id: Identifier, args: Seq[ValueExp]) extends TpeConstructor
case object TpeConstructorStar extends TpeConstructor

case object BlankLine extends TopLevel with BodyStmt

case class Import(path: Identifier, imported: Option[SBFile]) extends TopLevel


case class Comment(commentText: String) extends TopLevel with BodyStmt

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp) extends TopLevel

case class ConstructorDef(id: Identifier,
                          args: Seq[LocalName],
                          cstrApp: ConstructorApp) extends TopLevel

case class NestedInstance(nested: InstanceExp) extends BodyStmt

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[BodyStmt]) extends BodyStmt

case class SBFile(tops: Seq[TopLevel] = Seq.empty, rdfAbout: Option[Url] = None, source: Option[Url] = None)