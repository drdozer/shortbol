package uk.co.turingatemyhamster
package shortbol


import datatree._
import relations._
import web._

import ast.{SBFile, TopLevel}
import ops.{EvalContext, PrettyPrinter, ShortbolParser}
import pragma._

object Fixture {

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  def eval(file: SBFile): (EvalContext, Seq[TopLevel.InstanceExp]) =
    eval(file, emptyContext)

  def eval(file: SBFile, ctxt: EvalContext): (EvalContext, Seq[TopLevel.InstanceExp]) =
    file.eval.run(ctxt)

  def prettyPrinter(out: Appendable): PrettyPrinter = PrettyPrinter(out)

  lazy val emptyContext: EvalContext = EvalContext()

  lazy val configuredContext: EvalContext = emptyContext.withPHooks(
    Import(Resolver.fromWeb).pHook)

//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}