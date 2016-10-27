package uk.co.turingatemyhamster.shortbol
package longhandAst

case class SBFile(tops: Seq[InstanceExp]) extends shorthandAst.AstNode

case class InstanceExp(identifier: shorthandAst.Identifier,
                       cstrApp: ConstructorApp) extends shorthandAst.AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[PropertyExp]) extends shorthandAst.AstNode

object ConstructorApp {
  def apply[T](cstr: T, props: PropertyExp*)(implicit eT: T => TpeConstructor): ConstructorApp =
    ConstructorApp(cstr, props)
}

case class PropertyExp(property: shorthandAst.Identifier, value: shorthandAst.PropertyValue) extends shorthandAst.AstNode

case class TpeConstructor(tpe: shorthandAst.Identifier) extends shorthandAst.AstNode

object sugar {
  import scala.language.implicitConversions
  import shorthandAst.sugar.IvPair

  implicit def l_idToTpe[I](i: I)(implicit e: I => shorthandAst.Identifier): TpeConstructor =
    TpeConstructor(i)

  implicit def l_toProperty[I, V](iv: I IvPair V)
                                (implicit iE: I => shorthandAst.Identifier, vE: V => shorthandAst.PropertyValue): PropertyExp =
    PropertyExp(iv.i, iv.v)
}