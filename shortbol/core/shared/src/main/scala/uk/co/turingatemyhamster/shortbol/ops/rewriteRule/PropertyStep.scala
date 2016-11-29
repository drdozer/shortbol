package uk.co.turingatemyhamster.shortbol.ops.rewriteRule

import uk.co.turingatemyhamster.shortbol.longhandAst.{ConstructorApp, PropertyExp, PropertyValue}
import uk.co.turingatemyhamster.shortbol.ops.{RewriteAt, RewriteAtBuilder, RewriteRule}
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.RepairOps.{ConstructorAppPath, ReferencePath}
import uk.co.turingatemyhamster.shortbol.shorthandAst.Identifier


case class PropertyStep [F, G](f: F, g: G)

object PropertyStep {

  implicit class PropertyStepOps[F](_f: F) {
    def --> [G](g: G): PropertyStep[F, G] = PropertyStep(_f, g)
  }


  implicit def stepToReferencePath[F, G](implicit
                                         pathF: ConstructorAppPath[F],
                                         pathG: ReferencePath[G]): ReferencePath[PropertyStep[F, G]] = new ReferencePath[PropertyStep[F, G]] {
    def apply(step: PropertyStep[F, G], pe: PropertyExp) =
      for {
        app <- pathF(step.f, pe)
        b <- app.body
        id <- pathG(step.g, b)
      } yield id
  }

  implicit def stepToConstructorAppPath[F, G](implicit
                                              pathF: ConstructorAppPath[F],
                                              pathG: ConstructorAppPath[G]): ConstructorAppPath[PropertyStep[F, G]] = new ConstructorAppPath[PropertyStep[F, G]] {
    override def apply(step: PropertyStep[F, G], pe: PropertyExp) =
      for {
        app <- pathF(step.f, pe)
        b <- app.body
        app2 <- pathG(step.g, b)
      } yield app2
  }

  implicit def stepToRewriteBuilder[F, G, A](implicit
                                                   builderF: RewriteAtBuilder[F, List[PropertyExp], List[PropertyExp]],
                                                   builderG: RewriteAtBuilder[G, List[PropertyExp], A]): RewriteAtBuilder[PropertyStep[F, G], List[PropertyExp], A] = new RewriteAtBuilder[PropertyStep[F, G], List[PropertyExp], A] {
    override def apply(at: PropertyStep[F, G]) = new RewriteAt[List[PropertyExp], A] {
      override def apply(rr: RewriteRule[A]) = rr at at.g at at.f
    }
  }
}