package uk.co.turingatemyhamster.shortbol
package longhandAst

case class SBFile(tops: Seq[shorthandAst.TopLevel.InstanceExp]) extends shorthandAst.AstNode
