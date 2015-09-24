package uk.co.turingatemyhamster
package shortbol

import datatree._
import relations._
import web._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait Fixture {

  def expand(file: SBFile): (ExpansionContext, List[SBFile])
  def expand(file: SBFile, ctxt: ExpansionContext): (ExpansionContext, List[SBFile])

  def resolver: Resolver

  def emptyContext: ExpansionContext

  def prettyPrinter(out: Appendable): PrettyPrinter

  def parser: ShortbolParser.type

  def toDatatree[DT <: Datatree](file: SBFile)(implicit dsl: DatatreeDSL[DT],
                                               webDsl: WebDSL[DT],
                                               relDsl: RelationsDSL[DT]): DT#DocumentRoot
}

object Fixture extends FixtureProvider