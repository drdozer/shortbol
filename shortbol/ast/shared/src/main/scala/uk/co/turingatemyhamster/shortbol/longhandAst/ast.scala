package uk.co.turingatemyhamster.shortbol
package longhandAst

import monocle.Traversal
import monocle.function.Each

case class SBFile(tops: List[InstanceExp]) extends shorthandAst.AstNode

case class InstanceExp(identifier: shorthandAst.Identifier,
                       cstrApp: ConstructorApp) extends shorthandAst.AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: List[PropertyExp]) extends shorthandAst.AstNode

object ConstructorApp {
  def apply[T](cstr: T, props: List[PropertyExp]*)(implicit eT: T => TpeConstructor): ConstructorApp =
    new ConstructorApp(cstr, props.to[List].flatten) // new keyword required to force the case-class constructor
}

sealed trait PropertyValue

object PropertyValue {
  case class Literal(value: shorthandAst.Literal) extends PropertyValue
  case class Reference(value: shorthandAst.Identifier) extends PropertyValue
  case class Nested(value: longhandAst.ConstructorApp) extends PropertyValue
}

case class PropertyExp(property: shorthandAst.Identifier, value: longhandAst.PropertyValue) extends shorthandAst.AstNode

case class TpeConstructor(tpe: shorthandAst.Identifier) extends shorthandAst.AstNode

object sugar {
  import scala.language.implicitConversions
  import shorthandAst.sugar.IvPair

  implicit def l_idToTpe[I](i: I)(implicit e: I => shorthandAst.Identifier): TpeConstructor =
    TpeConstructor(i)

  implicit def l_toProperty[I, V](iv: I IvPair V)
                                (implicit iE: I => shorthandAst.Identifier, vE: V => longhandAst.PropertyValue): List[PropertyExp] =
    PropertyExp(iv.i, iv.v) :: Nil

  implicit def l_toProperties[T[_], I, V](iv: I IvPair T[V])
                                (implicit pp: (I IvPair V) => List[PropertyExp], tr: Each[T[V], V]): List[PropertyExp] =
    for {
      v <- tr.each.getAll(iv.v)
      p <- IvPair(iv.i, v)
    } yield p


  implicit def l_pvLiteral[L](l: L)(implicit lE: L => shorthandAst.Literal): PropertyValue = PropertyValue.Literal(l)
  implicit def l_pvReference[R](r: R)(implicit rE: R => shorthandAst.Identifier): PropertyValue = PropertyValue.Reference(r)
  implicit def l_pvNested(n: ConstructorApp): PropertyValue = PropertyValue.Nested(n)
}