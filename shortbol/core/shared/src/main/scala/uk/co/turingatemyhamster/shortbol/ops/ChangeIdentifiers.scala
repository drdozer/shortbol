package uk.co.turingatemyhamster.shortbol.ops

import scalaz.Scalaz._
import shapeless._
import uk.co.turingatemyhamster.shortbol.ast
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
  case class at(transform: ast.Identifier => EvalState[ast.Identifier]) extends TypeClassCompanion[ChangeIdentifiers]
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

    implicit def deriveNodeInstance[F <: ast.AstNode, G]
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

    implicit val identifier: ChangeIdentifiers[ast.Identifier] = new ChangeIdentifiers[ast.Identifier] {
      override def apply(t: ast.Identifier) = transform(t)
    }
    implicit val style = self[ast.StringLiteral.Style]
    implicit val literal = self[ast.Literal]
    implicit val assignment = self[ast.Assignment]
    implicit val tpeConstructor = self[ast.TpeConstructor]
    implicit val instanceExp = self[ast.InstanceExp]
    implicit val constructorApp = self[ast.ConstructorApp]
    implicit val constructorDef = self[ast.ConstructorDef]
    implicit val valueExp = self[ast.ValueExp]
    implicit val propertyValue = self[ast.PropertyValue]
    implicit val propertyExp = self[ast.PropertyExp]
    implicit val bodyStmt = self[ast.BodyStmt]
    implicit val topLevel = self[ast.TopLevel]
    implicit val sbFile = self[ast.SBFile]

    def miss[T]: ChangeIdentifiers[T] = new MissIdentifiers[T]

    class MissIdentifiers[T] extends ChangeIdentifiers[T] {
      override def apply(t: T) = t.point[EvalState]
    }
  }
}