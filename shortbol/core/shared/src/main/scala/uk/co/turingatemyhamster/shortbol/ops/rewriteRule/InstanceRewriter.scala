package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import longhandAst.{InstanceExp, SBFile}
import Eval.EvalState
import RewriteRule.allElements
import optics.longhand.SBFile._

import scalaz._
import Scalaz._

trait InstanceRewriter {
  def instanceRewrite: RewriteRule[InstanceExp]
}


object InstanceRewriter {
  def updatingInstances(ri: RewriteRule[InstanceExp]): RewriteRule[InstanceExp] = RewriteRule { (ie: InstanceExp) =>
    for {
      is <- ri(ie)
      _ <- is.fold(
        _ => ().point[EvalState],
        i => modify((_: EvalContext).withInstances(i.run._2)))
    } yield is
  }

  def rewriteFile(irs: InstanceRewriter*): RewriteRule[SBFile] =
    irs.map(ir => updatingInstances(ir.instanceRewrite)).map(_ at allElements at tops).reduce(_ andThen _)
}
