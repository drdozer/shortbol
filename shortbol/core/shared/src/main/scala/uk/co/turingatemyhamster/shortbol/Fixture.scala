package uk.co.turingatemyhamster
package shortbol


import shorthandAst.Pragma
import sharedAst.sugar._
import shorthandAst.sugar._
import ops.{EvalContext, RewriteRule, ShortbolParser}
import ShortbolParser.POps
import pragma._
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule._

import scalaz._


object Fixture {

  import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps

  lazy val emptyContext: EvalContext = EvalContext()

  lazy val configuredContext: EvalContext =
    bootstrap.exec(emptyContext)

  lazy val bootstrapPragmas = PragmaPragma(
    ImportBaseUrl.apply,
    ImportPragma(Resolver.cache(Resolver.fromWeb)),
    DefaultPrefixPragma.apply,
    PrefixPragma.apply
  )

  lazy val bootstrap = for {
    _ <- bootstrapPragmas.register(Pragma("pragma", List("pragma")))
    _ <- ShortbolParser.SBFile.withPositions("_bootstrap_", bootstrapPragmas.bootstrap).get.value.eval
    _ <- ShortbolParser.SBFile.withPositions("_peramble_", preamble).get.value.eval
  } yield ()

  lazy val preamble =
    """@prefix stdlib <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/>
      |""".stripMargin

  lazy val fixup = InstanceRewriter.rewriteFile(
    RepairSequence, RepairComponentDefinition, RepairModule, RepairIdentities)

  lazy val doFixup = RewriteRule.rewrite(fixup, _: longhandAst.SBFile)
}