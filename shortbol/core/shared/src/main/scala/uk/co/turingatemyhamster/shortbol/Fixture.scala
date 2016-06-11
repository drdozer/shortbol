package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import web._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait Fixture {

  def eval(file: SBFile): (EvalContext, List[SBFile])
  def eval(file: SBFile, ctxt: EvalContext): (EvalContext, List[SBFile])

  def resolver: Resolver

  def emptyContext: EvalContext

  def prettyPrinter(out: Appendable): PrettyPrinter

  def parser: ShortbolParser.type
//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}

object Fixture extends FixtureProvider