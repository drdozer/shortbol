package uk.co.turingatemyhamster.shortbol
package shorthandAst

sealed trait TopLevel
sealed trait BodyStmt
sealed trait ValueExp
sealed trait TpeConstructor extends sharedAst.AstNode

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
  case class Identifier(identifier: sharedAst.Identifier) extends ValueExp
  case class Literal(literal: sharedAst.Literal) extends ValueExp
}


// type constructors
case class TpeConstructor1(id: sharedAst.Identifier, args: List[ValueExp]) extends TpeConstructor
case class TpeConstructorStar() extends TpeConstructor

// expressions
case class Assignment(property: sharedAst.Identifier, value: ValueExp) extends sharedAst.AstNode

case class BlankLine() extends sharedAst.AstNode

case class Comment(commentText: String) extends sharedAst.AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: List[BodyStmt]) extends sharedAst.AstNode

object ConstructorApp {
  def apply[T](cstr: T, bodys: BodyStmt*)(implicit e: T => TpeConstructor): ConstructorApp =
    ConstructorApp(e(cstr), bodys.to[List])
}

case class ConstructorDef(id: sharedAst.Identifier,
                            args: List[sharedAst.Identifier],
                            cstrApp: ConstructorApp) extends sharedAst.AstNode

case class InstanceExp(identifier: sharedAst.Identifier,
                       cstrApp: ConstructorApp) extends sharedAst.AstNode

case class Pragma(id: sharedAst.Identifier, values: List[shorthandAst.ValueExp]) extends sharedAst.AstNode

// the whole thing
case class SBFile(tops: List[TopLevel]) extends sharedAst.AstNode

case class PropertyExp(property: sharedAst.Identifier, value: PropertyValue) extends sharedAst.AstNode

sealed trait PropertyValue

object PropertyValue {
  case class Literal(value: sharedAst.Literal) extends PropertyValue
  case class Reference(value: sharedAst.Identifier) extends PropertyValue
  case class Nested(value: shorthandAst.ConstructorApp) extends PropertyValue
}

object sugar {
  import scala.language.implicitConversions
  import sharedAst.sugar.IvPair

  implicit def tlAssignment[A](a: A)(implicit e: A => shorthandAst.Assignment): TopLevel.Assignment = TopLevel.Assignment(a)
  implicit def tlBlankLine(bl: BlankLine): TopLevel.BlankLine = TopLevel.BlankLine(bl)
  implicit def tlComment(c: Comment): TopLevel.Comment = TopLevel.Comment(c)
  implicit def tlConstructorDef(c: ConstructorDef): TopLevel.ConstructorDef = TopLevel.ConstructorDef(c)

  implicit def idToTpe[I](i: I)(implicit e: I => sharedAst.Identifier): TpeConstructor1 =
    TpeConstructor1(i, Nil)

  implicit class IdentifierToTpe[I](_i: I) {
    def withArgs(ves: ValueExp*)(implicit iE: I => sharedAst.Identifier): TpeConstructor =
      TpeConstructor1(_i, ves.to[List])
  }

  implicit def bsBlankLine(bl: BlankLine): BodyStmt.BlankLine = BodyStmt.BlankLine(bl)
  implicit def bsComment(c: Comment): BodyStmt.Comment = BodyStmt.Comment(c)
  implicit def bsPropertyExp[PE](pe: PE)(implicit peE: PE => PropertyExp): BodyStmt.PropertyExp = BodyStmt.PropertyExp(pe)

  implicit def pvLiteral[L](l: L)(implicit lE: L => sharedAst.Literal): PropertyValue = PropertyValue.Literal(l)
  implicit def pvReference[R](r: R)(implicit rE: R => sharedAst.Identifier): PropertyValue = PropertyValue.Reference(r)
  implicit def pvNested(n: shorthandAst.ConstructorApp): PropertyValue = PropertyValue.Nested(n)

  implicit def veIdentifier[I](i: I)(implicit e: I => sharedAst.Identifier): ValueExp.Identifier = ValueExp.Identifier(i)
  implicit def veLiteral[L](l: L)(implicit e: L => sharedAst.Literal): ValueExp.Literal = ValueExp.Literal(l)
  def slVal(s: String): ValueExp = veLiteral(sharedAst.sugar.slLit(s))

  implicit def toAssignment[I, V](iv: I IvPair V)(implicit iE: I => sharedAst.Identifier, vE: V => ValueExp): shorthandAst.Assignment =
    shorthandAst.Assignment(iv.i, iv.v)
  implicit def toProperty[I, V](iv: I IvPair V)(implicit iE: I => sharedAst.Identifier, vE: V => PropertyValue): PropertyExp =
    PropertyExp(iv.i, iv.v)
}

