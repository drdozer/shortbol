package uk.co.turingatemyhamster
package shortbol

import datatree._
import web._
import relations._
import uk.co.turingatemyhamster.shortbol.ast.{SBFile, TopLevel}
import uk.co.turingatemyhamster.shortbol.ops.{EvalContext, PrettyPrinter, Resolver, ShortbolParser}

///**
// * Created by nmrp3 on 08/09/15.
// */
trait FixtureProvider extends Fixture {
  self =>

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  override def eval(file: SBFile): (EvalContext, Seq[TopLevel.InstanceExp]) =
    eval(file, emptyContext)

  override def eval(file: SBFile, ctxt: EvalContext): (EvalContext, Seq[TopLevel.InstanceExp]) =
    file.eval.run(ctxt)

  override def resolver: Resolver = new ResolverProvider {
    override protected def parser: ShortbolParser.type = self.parser
  }

  override def prettyPrinter(out: Appendable): PrettyPrinter = PrettyPrinter(out)

  override lazy val emptyContext: EvalContext = EvalContext(rslvr = resolver)

  override def parser: ShortbolParser.type = ShortbolParser
//
//  override def toDatatree[DT <: Datatree](file: SBFile)
//                                         (implicit ee: ExporterEnv[DT]): DT#DocumentRoot = {
//    import Exporter._
//    import ee._
//    file.export
//  }
}
