package uk.co.turingatemyhamster.shortbol
package longhandAst

import monocle.function.Each

case class SBFile(tops: List[InstanceExp]) extends sharedAst.AstNode

case class InstanceExp(identifier: sharedAst.Identifier,
                       cstrApp: ConstructorApp) extends sharedAst.AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: List[PropertyExp]) extends sharedAst.AstNode

sealed trait PropertyValue

object PropertyValue {
  case class Literal(value: sharedAst.Literal) extends PropertyValue
  case class Reference(value: sharedAst.Identifier) extends PropertyValue
  case class Nested(value: ConstructorApp) extends PropertyValue
}

case class PropertyExp(property: sharedAst.Identifier, value: PropertyValue) extends sharedAst.AstNode

case class TpeConstructor(tpe: sharedAst.Identifier) extends sharedAst.AstNode

object sugar {
  import scala.language.implicitConversions
  import sharedAst.sugar.IvPair

  implicit class ConstructorAppSugar[T](val _cstr: T) {
    def apply(props: List[PropertyExp]*)(implicit eT: T => TpeConstructor): ConstructorApp =
      ConstructorApp(_cstr, props.to[List].flatten)
  }

  implicit def l_idToTpe[I](i: I)(implicit e: I => sharedAst.Identifier): TpeConstructor =
    TpeConstructor(i)

  implicit def l_toProperty[I, V](iv: I IvPair V)
                                (implicit iE: I => sharedAst.Identifier, vE: V => PropertyValue): List[PropertyExp] =
    PropertyExp(iv.i, iv.v) :: Nil

  implicit def l_toProperties[T[_], I, V](iv: I IvPair T[V])
                                (implicit pp: (I IvPair V) => List[PropertyExp], tr: Each[T[V], V]): List[PropertyExp] =
    for {
      v <- tr.each.getAll(iv.v)
      p <- IvPair(iv.i, v)
    } yield p


  implicit def l_pvLiteral[L](l: L)(implicit lE: L => sharedAst.Literal): PropertyValue = PropertyValue.Literal(l)
  implicit def l_pvReference[R](r: R)(implicit rE: R => sharedAst.Identifier): PropertyValue = PropertyValue.Reference(r)
  implicit def l_pvNested(n: ConstructorApp): PropertyValue = PropertyValue.Nested(n)
}

