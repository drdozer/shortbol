package uk.co.turingatemyhamster.shortbol.ops

import scalaz.Scalaz._
import shapeless._
import uk.co.turingatemyhamster.shortbol.shorthandAst
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState

/**
  *
  *
  * @author Matthew Pocock
  */
trait ChangeIdentifiers[T] {
  def apply(t: T): EvalState[T]
}

object ChangeIdentifiers {
  case class at(transform: shorthandAst.Identifier => EvalState[shorthandAst.Identifier]) extends TypeClassCompanion[ChangeIdentifiers]
  {
    self =>

    override val typeClass: TypeClass[ChangeIdentifiers] = new TypeClass[ChangeIdentifiers] {
      override def coproduct[L, R <: Coproduct](cl: => ChangeIdentifiers[L],
                                                cr: => ChangeIdentifiers[R]) =
          new ChangeIdentifiers[:+:[L, R]] {
            override def apply(t: :+:[L, R]) = t match {
              case Inl(l) => for {
                cll <- cl(l)
              } yield Inl(cll)
              case Inr(r) => for {
                crr <- cr(r)
              } yield Inr(crr)
            }
          }


      override def emptyCoproduct = new ChangeIdentifiers[CNil] {
        override def apply(t: CNil) = ???
      }

      override def product[H, T <: HList](ch: ChangeIdentifiers[H],
                                          ct: ChangeIdentifiers[T]) = new ChangeIdentifiers[::[H, T]] {
        override def apply(t: ::[H, T]) = for {
          chh <- ch(t.head)
          ctt <- ct(t.tail)
        } yield chh :: ctt
      }

      override def emptyProduct = miss[HNil]

      override def project[F, G](instance: => ChangeIdentifiers[G],
                                 to: (F) => G,
                                 from: (G) => F) = new ChangeIdentifiers[F] {
        override def apply(t: F) = for {
          tot <- instance(to(t))
        } yield from(tot)
      }
    }

    implicit def deriveNodeInstance[F <: shorthandAst.AstNode, G]
    (implicit gen: Generic.Aux[F, G], cg: Lazy[ChangeIdentifiers[G]]): ChangeIdentifiers[F] = {
      val fg = typeClass.project(cg.value, gen.to _, gen.from _)
      new ChangeIdentifiers[F] {
        override def apply(t: F) = for {
          ff <- fg.apply(t)
        } yield {
          ff.region = t.region
          ff
        }
      }
    }

    implicit def seq[T](implicit e: ChangeIdentifiers[T]): ChangeIdentifiers[Seq[T]] =
      typeClass.project[Seq[T], List[T]](implicitly[ChangeIdentifiers[List[T]]], _.to[List], identity)

    implicit val missString = miss[String]
    implicit val missInt = miss[Int]
    implicit val missBoolean = miss[Boolean]

    implicit val identifier: ChangeIdentifiers[shorthandAst.Identifier] = new ChangeIdentifiers[shorthandAst.Identifier] {
      override def apply(t: shorthandAst.Identifier) = transform(t)
    }
    implicit val style = self[shorthandAst.StringLiteral.Style]
    implicit val literal = self[shorthandAst.Literal]
    implicit val assignment = self[shorthandAst.Assignment]
    implicit val tpeConstructor = self[shorthandAst.TpeConstructor]
    implicit val instanceExp = self[shorthandAst.InstanceExp]
    implicit val constructorApp = self[shorthandAst.ConstructorApp]
    implicit val constructorDef = self[shorthandAst.ConstructorDef]
    implicit val valueExp = self[shorthandAst.ValueExp]
    implicit val propertyValue = self[shorthandAst.PropertyValue]
    implicit val propertyExp = self[shorthandAst.PropertyExp]
    implicit val bodyStmt = self[shorthandAst.BodyStmt]
    implicit val topLevel = self[shorthandAst.TopLevel]
    implicit val sbFile = self[shorthandAst.SBFile]

    def miss[T]: ChangeIdentifiers[T] = new MissIdentifiers[T]

    class MissIdentifiers[T] extends ChangeIdentifiers[T] {
      override def apply(t: T) = t.point[EvalState]
    }
  }
}