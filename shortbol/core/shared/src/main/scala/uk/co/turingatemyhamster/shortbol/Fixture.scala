package uk.co.turingatemyhamster
package shortbol


import datatree._
import relations._
import web._
import ast.{SBFile, TopLevel, Pragma}
import ast.sugar._
import ops.{EvalContext, ShortbolParser, Eval}
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

  lazy val configuredContext: EvalContext =
    bootstrap.exec(emptyContext)

  lazy val bootstrapPragmas = PragmaPragma(
      ImportPragma(Resolver.fromWeb),
      DefaultPrefixPragma.apply,
      PrefixPragma.apply
  )

  lazy val bootstrap = for {
    _ <- bootstrapPragmas.register(Pragma("pragma", Seq("pragma")))
    _ <- ShortbolParser.SBFile.parse(bootstrapPragmas.bootstrap).get.value.eval
  } yield ()

//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}