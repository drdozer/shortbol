package uk.co.turingatemyhamster.shortbol

sealed trait Identifier extends ValueExp

case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String)
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier
//case class QuotedIdentifier(quote: String) extends Identifier

sealed trait ValueExp
case class StringLiteral(s: String, escaped: Boolean = false) extends ValueExp {
  def isEscaped = escaped || s.contains("\"")
}

case class MultiLineLiteral(ss: Seq[String], indent: Int) extends ValueExp

case class IntegerLiteral(i: Int) extends ValueExp

sealed trait BodyStmt

case class Assignment(property: Identifier, value: ValueExp) extends BodyStmt with TopLevel

case class NestedInstance(nested: InstanceExp) extends BodyStmt

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[BodyStmt]) extends BodyStmt

sealed trait TpeConstructor

case class TpeConstructor1(id: Identifier, args: Seq[ValueExp]) extends TpeConstructor
case object TpeConstructorStar extends TpeConstructor

sealed trait TopLevel

case object BlankLine extends TopLevel with BodyStmt

trait Import extends TopLevel

case class UnprocessedImport(path: Identifier) extends Import

case class ProcessedImport(path: Identifier, imported: SBFile) extends Import

case class Comment(commentText: String) extends TopLevel with BodyStmt

case class InstanceExp(id: Identifier,
                       cstrApp: ConstructorApp) extends TopLevel

case class ConstructorDef(id: Identifier,
                          args: Seq[LocalName],
                          cstrApp: ConstructorApp) extends TopLevel

case class SBFile(tops: Seq[TopLevel] = Seq(), rdfAbout: Option[Url] = None, source: Option[Url] = None)