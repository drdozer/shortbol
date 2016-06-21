package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import uk.co.turingatemyhamster.shortbol.ast.{SBFile, TopLevel}
import uk.co.turingatemyhamster.shortbol.ops.{EvalContext, PrettyPrinter, Resolver, ShortbolParser}
import web._


object Fixture {

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  def eval(file: SBFile): (EvalContext, Seq[TopLevel.InstanceExp]) =
    eval(file, emptyContext)

  def eval(file: SBFile, ctxt: EvalContext): (EvalContext, Seq[TopLevel.InstanceExp]) =
    file.eval.run(ctxt)

  def prettyPrinter(out: Appendable): PrettyPrinter = PrettyPrinter(out)

  lazy val emptyContext: EvalContext = EvalContext(rslvr = resolver)

  def parser: ShortbolParser.type = ShortbolParser

  object resolver extends ResolverProvider
//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}