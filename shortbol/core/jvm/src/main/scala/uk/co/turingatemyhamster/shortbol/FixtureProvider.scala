package uk.co.turingatemyhamster
package shortbol

import datatree._
import web._
import relations._

/**
 * Created by nmrp3 on 08/09/15.
 */
trait FixtureProvider extends Fixture {
  self =>


  import Expander.ops._

  override def expand(file: SBFile): (ExpansionContext, List[SBFile]) =
    expand(file, emptyContext)

  override def expand(file: SBFile, ctxt: ExpansionContext): (ExpansionContext, List[SBFile]) =
    file.expansion.run(ctxt)

  override def resolver: Resolver = new ResolverProvider {
    override protected def parser: ShortbolParser.type = self.parser
  }

  override def prettyPrinter(out: Appendable): PrettyPrinter = PrettyPrinter(out)

  override lazy val emptyContext: ExpansionContext = ExpansionContext(rslvr = resolver)

  override def parser: ShortbolParser.type = ShortbolParser

  override def toDatatree[DT <: Datatree](file: SBFile)
                                         (implicit dtDsl: DatatreeDSL[DT],
                                          webDsl: WebDSL[DT],
                                          relDsl: RelationsDSL[DT]): DT#DocumentRoot = Exporter.export(file)
}
