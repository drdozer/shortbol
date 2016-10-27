package uk.co.turingatemyhamster.shortbol
package longhandAst

case class SBFile(tops: Seq[InstanceExp]) extends shorthandAst.AstNode

case class InstanceExp(identifier: shorthandAst.Identifier,
                       cstrApp: ConstructorApp) extends shorthandAst.AstNode

case class ConstructorApp(cstr: TpeConstructor,
                          body: Seq[shorthandAst.BodyStmt]) extends shorthandAst.AstNode

object ConstructorApp {
  def apply[T](cstr: T, bodys: shorthandAst.BodyStmt*)(implicit eT: T => TpeConstructor): ConstructorApp =
    ConstructorApp(cstr, bodys)
}

case class TpeConstructor(tpe: shorthandAst.Identifier) extends shorthandAst.AstNode

object sugar {
  import scala.language.implicitConversions

  implicit def l_idToTpe[I](i: I)(implicit e: I => shorthandAst.Identifier): TpeConstructor =
    TpeConstructor(i)
}