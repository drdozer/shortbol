package uk.co.turingatemyhamster
package shortbol


import datatree._
import relations._
import web._
import ast.{SBFile, TopLevel, Pragma}
import ast.sugar._
import ops.{EvalContext, ShortbolParser}
import pragma._

import scalaz.Scalaz._
import scalaz._


object Fixture {

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  def eval(file: SBFile): (EvalContext, Seq[TopLevel.InstanceExp]) =
    eval(file, emptyContext)

  def eval(file: SBFile, ctxt: EvalContext): (EvalContext, Seq[TopLevel.InstanceExp]) =
    file.eval.run(ctxt)

  lazy val emptyContext: EvalContext = EvalContext()

  lazy val configuredContext: EvalContext = (
    for {
      _ <- bootstrapPragmas
      _ <- bootstrapScript
    } yield ()).exec(emptyContext)

  lazy val bootstrapPragmas = PragmaPragma(
    Map(
      "import" -> ImportPragma(Resolver.fromWeb),
      "defaultPrefix" -> DefaultPrefixPragma.apply,
      "prefix" -> PrefixPragma.apply
    )
  ).register(Pragma("pragma", Nil))

  lazy val bootstrapScript = ShortbolParser.SBFile.parse(
    """# register the import pragma
      |@pragma import url
      |@pragma defaultPrefix prefixName
      |@pragma prefix prefixName url
    """.stripMargin).get.value.eval

//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}