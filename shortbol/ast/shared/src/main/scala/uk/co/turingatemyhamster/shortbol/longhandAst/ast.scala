package uk.co.turingatemyhamster.shortbol
package longhandAst

case class SBFile(tops: Seq[InstanceExp]) extends shorthandAst.AstNode

case class InstanceExp(id: shorthandAst.Identifier,
                       cstrApp: shorthandAst.ConstructorApp) extends shorthandAst.AstNode