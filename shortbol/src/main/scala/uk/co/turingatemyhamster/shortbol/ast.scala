package uk.co.turingatemyhamster.shortbol

trait Identifier extends ValueExp

case class LocalName(name: String) extends Identifier
case class NSPrefix(pfx: String)
case class QName(prefix: NSPrefix, localName: LocalName) extends Identifier
case class Url(url: String) extends Identifier

trait ValueExp
case class StringLiteral(s: String) extends ValueExp
case class IntegerLiteral(i: Int) extends ValueExp

case class Assignment(property: Identifier, value: ValueExp) extends BodyStmt

case class NestedAssignment(property: Identifier, body: Seq[BodyStmt]) extends BodyStmt

case class NestedInstance(nested: InstanceExp) extends BodyStmt

case class InstanceExp(id: Identifier, cstr: TpeConstructor, body: Seq[BodyStmt])

trait BodyStmt

case class TpeConstructor(id: Identifier, args: Seq[ValueExp])

case class ConstructorDef(id: Identifier, args: Seq[LocalName], cstr: TpeConstructor, body: Seq[BodyStmt])