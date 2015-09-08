package uk.co.turingatemyhamster.shortbol

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

}

object Fixture extends FixtureProvider