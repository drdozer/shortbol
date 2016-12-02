package uk.co.turingatemyhamster.shortbol
package shorthandAst

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
  case class Assignment(assignment: shorthandAst.Assignment) extends TopLevel
  case class BlankLine(blankLine: shorthandAst.BlankLine) extends TopLevel
  case class Comment(comment: shorthandAst.Comment) extends TopLevel
  case class ConstructorDef(constructorDef: shorthandAst.ConstructorDef) extends TopLevel
  case class InstanceExp(instanceExp: shorthandAst.InstanceExp) extends TopLevel
  case class Pragma(pragma: shorthandAst.Pragma) extends TopLevel
}

// body statements
object BodyStmt {
  case class BlankLine(blankLine: shorthandAst.BlankLine) extends BodyStmt
  case class Comment(comment: shorthandAst.Comment) extends BodyStmt
  case class PropertyExp(propertyExp: shorthandAst.PropertyExp) extends BodyStmt
}

// value expressions
object ValueExp {
  case class Identifier(identifier: shorthandAst.Identifier) extends ValueExp
  case class Literal(literal: shorthandAst.Literal) extends ValueExp
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

  case class MultiLine(ss: List[String], indent: Int) extends Style {
    def asString = ss mkString "\n"
  }
}

case class IntegerLiteral(i: Int) extends Literal

case class Datatype(tpe: Identifier) extends AstNode
case class Language(tag: String) extends AstNode

// type constructors
case class TpeConstructor1(id: Identifier, args: List[ValueExp]) extends TpeConstructor
case class TpeConstructorStar() extends TpeConstructor

// expressions
case class Assignment(property: Identifier, value: ValueExp) extends AstNode

case class BlankLine() extends AstNode

case class Comment(commentText: String) extends AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: List[BodyStmt]) extends AstNode

object ConstructorApp {
  def apply[T](cstr: T, bodys: BodyStmt*)(implicit e: T => TpeConstructor): ConstructorApp =
    ConstructorApp(e(cstr), bodys.to[List])
}

case class ConstructorDef(id: Identifier,
                            args: List[Identifier],
                            cstrApp: ConstructorApp) extends AstNode

case class InstanceExp(identifier: Identifier,
                       cstrApp: ConstructorApp) extends AstNode

case class Pragma(id: Identifier, values: List[shorthandAst.ValueExp]) extends AstNode

// the whole thing
case class SBFile(tops: List[TopLevel]) extends AstNode

case class PropertyExp(property: Identifier, value: PropertyValue) extends AstNode

sealed trait PropertyValue

object PropertyValue {
  case class Literal(value: shorthandAst.Literal) extends PropertyValue
  case class Reference(value: shorthandAst.Identifier) extends PropertyValue
  case class Nested(value: shorthandAst.ConstructorApp) extends PropertyValue
}

object sugar {
  import scala.language.implicitConversions

  implicit def tlAssignment[A](a: A)(implicit e: A => shorthandAst.Assignment): TopLevel.Assignment = TopLevel.Assignment(a)
  implicit def tlBlankLine(bl: BlankLine): TopLevel.BlankLine = TopLevel.BlankLine(bl)
  implicit def tlComment(c: Comment): TopLevel.Comment = TopLevel.Comment(c)
  implicit def tlConstructorDef(c: ConstructorDef): TopLevel.ConstructorDef = TopLevel.ConstructorDef(c)

  implicit def idToTpe[I](i: I)(implicit e: I => Identifier): TpeConstructor1 =
    TpeConstructor1(i, Nil)

  implicit class IdentifierToTpe[I](_i: I) {
    def withArgs(ves: ValueExp*)(implicit iE: I => Identifier): TpeConstructor =
      TpeConstructor1(_i, ves.to[List])
  }

  implicit def bsBlankLine(bl: BlankLine): BodyStmt.BlankLine = BodyStmt.BlankLine(bl)
  implicit def bsComment(c: Comment): BodyStmt.Comment = BodyStmt.Comment(c)
  implicit def bsPropertyExp[PE](pe: PE)(implicit peE: PE => PropertyExp): BodyStmt.PropertyExp = BodyStmt.PropertyExp(pe)

  implicit def pvLiteral[L](l: L)(implicit lE: L => Literal): PropertyValue = PropertyValue.Literal(l)
  implicit def pvReference[R](r: R)(implicit rE: R => Identifier): PropertyValue = PropertyValue.Reference(r)
  implicit def pvNested(n: shorthandAst.ConstructorApp): PropertyValue = PropertyValue.Nested(n)

  implicit def veIdentifier[I](i: I)(implicit e: I => shorthandAst.Identifier): ValueExp.Identifier = ValueExp.Identifier(i)
  implicit def veLiteral[L](l: L)(implicit e: L => shorthandAst.Literal): ValueExp.Literal = ValueExp.Literal(l)
  def slVal(s: String): ValueExp = veLiteral(slLit(s))

  implicit class IV[I](val _i: I) extends AnyVal {
    def := [V](v: V): I IvPair V = IvPair(_i, v)
  }

  case class IvPair [I, V](i: I, v: V)

  implicit def toAssignment[I, V](iv: I IvPair V)(implicit iE: I => shorthandAst.Identifier, vE: V => ValueExp): shorthandAst.Assignment =
    shorthandAst.Assignment(iv.i, iv.v)
  implicit def toProperty[I, V](iv: I IvPair V)(implicit iE: I => shorthandAst.Identifier, vE: V => PropertyValue): PropertyExp =
    PropertyExp(iv.i, iv.v)

  implicit def strLit[S](s: S)(implicit e: S => StringLiteral.Style): StringLiteral = StringLiteral(s, None, None)
  def slLit(s: String): StringLiteral = StringLiteral(StringLiteral.SingleLine(s, false), None, None)

  implicit def strLN(s: String): LocalName = LocalName(s)
  implicit def strNP(s: String): NSPrefix = NSPrefix(s)
  implicit def intIL(i: Int): IntegerLiteral = IntegerLiteral(i)

  implicit class NSPrefixOps[N](val _pfx: N) extends AnyVal {
    def :# (ln: LocalName)(implicit ne: N => NSPrefix): QName = QName(_pfx, ln)
  }

  def toLonghand(pv: PropertyValue): longhandAst.PropertyValue = pv match {
    case PropertyValue.Literal(lit) => longhandAst.PropertyValue.Literal(lit)
    case PropertyValue.Reference(ref) => longhandAst.PropertyValue.Reference(ref)
    case PropertyValue.Nested(nest) => ??? /* longhandAst.PropertyValue.Nested(toLonghand(nest)) */
  }
}