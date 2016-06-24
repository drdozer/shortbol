package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.ast

trait AllIdentifiers[T] {
  def apply(t: T): Seq[ast.Identifier]
}

object AllIdentifiers extends TypeClassCompanion[AllIdentifiers] {

  override val typeClass: TypeClass[AllIdentifiers] = new TypeClass[AllIdentifiers] {
    override def coproduct[L, R <: Coproduct](cl: => AllIdentifiers[L],
                                              cr: => AllIdentifiers[R]) = new AllIdentifiers[:+:[L, R]] {
      override def apply(t: :+:[L, R]) = t match {
        case Inl(l) => cl(l)
        case Inr(r) => cr(r)
      }
    }

    override def emptyCoproduct = new AllIdentifiers[CNil] {
      override def apply(t: CNil) = ???
    }

    override def product[H, T <: HList](ch: AllIdentifiers[H],
                                        ct: AllIdentifiers[T]) = new AllIdentifiers[::[H, T]] {
      override def apply(t: ::[H, T]) = ch(t.head) ++ ct(t.tail)
    }

    override def emptyProduct = new AllIdentifiers[HNil] {
      override def apply(t: HNil) = Seq.empty
    }

    override def project[F, G](instance: => AllIdentifiers[G],
                               to: (F) => G,
                               from: (G) => F) = new AllIdentifiers[F] {
      override def apply(t: F) = instance(to(t))
    }
  }

  implicit def seq[T](implicit e: AllIdentifiers[T]): AllIdentifiers[Seq[T]] = new AllIdentifiers[Seq[T]] {
    override def apply(t: Seq[T]) = t flatMap e.apply
  }

  implicit val missString = miss[String]
  implicit val missInt = miss[Int]
  implicit val missBoolean = miss[Boolean]
  implicit val missStyle = miss[ast.StringLiteral.Style]

  implicit val identifier: AllIdentifiers[ast.Identifier] = new AllIdentifiers[ast.Identifier] {
    override def apply(t: ast.Identifier) = Seq(t)
  }
  implicit val literal = AllIdentifiers[ast.Literal]
  implicit val tpeConstructor = AllIdentifiers[ast.TpeConstructor]
  implicit val valueExp = AllIdentifiers[ast.ValueExp]
  implicit val bodyStmt = AllIdentifiers[ast.BodyStmt]
  implicit val topLevel = AllIdentifiers[ast.TopLevel]
  implicit val sbFile = AllIdentifiers[ast.SBFile]

  def miss[T]: AllIdentifiers[T] = new AllIdentifiers[T] {
    override def apply(t: T) = Seq.empty
  }
}