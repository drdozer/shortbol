package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import uk.co.turingatemyhamster.shortbol.ast.{SBFile, TopLevel}
import uk.co.turingatemyhamster.shortbol.ops.{EvalContext, PrettyPrinter, Resolver, ShortbolParser}
import web._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait Fixture {

  def eval(file: SBFile): (EvalContext, Seq[TopLevel.InstanceExp])
  def eval(file: SBFile, ctxt: EvalContext): (EvalContext, Seq[TopLevel.InstanceExp])

  def resolver: Resolver

  def emptyContext: EvalContext

  def prettyPrinter(out: Appendable): PrettyPrinter

  def parser: ShortbolParser.type
//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}

object Fixture extends FixtureProvider