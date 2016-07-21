package uk.co.turingatemyhamster
package shortbol


import datatree._
import relations._
import web._
import ast.{Pragma, SBEvaluatedFile, SBFile}
import ast.sugar._
import ops.{Eval, EvalContext, ShortbolParser}
import ShortbolParser.POps
import pragma._

import scalaz.Scalaz._
import scalaz._


object Fixture {

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  lazy val emptyContext: EvalContext = EvalContext()

  lazy val configuredContext: EvalContext =
    bootstrap.exec(emptyContext)

  lazy val bootstrapPragmas = PragmaPragma(
    ImportBaseUrl.apply,
    ImportPragma(Resolver.fromWeb),
    DefaultPrefixPragma.apply,
    PrefixPragma.apply
  )

  lazy val bootstrap = for {
    _ <- bootstrapPragmas.register(Pragma("pragma", Seq("pragma")))
    _ <- ShortbolParser.SBFile.withPositions("_bootstrap_", bootstrapPragmas.bootstrap).get.value.eval
    _ <- ShortbolParser.SBFile.withPositions("_peramble_", preamble).get.value.eval
  } yield ()

  lazy val preamble =
    """@prefix stdlib <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/>
      |""".stripMargin

//
//  def toDatatree[DT <: Datatree](file: SBFile)(implicit  ee: ExporterEnv[DT]): DT#DocumentRoot
}