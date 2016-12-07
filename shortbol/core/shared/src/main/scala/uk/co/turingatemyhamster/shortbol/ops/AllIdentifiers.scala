package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.sharedAst._
import uk.co.turingatemyhamster.shortbol.shorthandAst

trait AllIdentifiers[T] {
  def apply(t: T): Seq[Identifier]
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

    override def emptyProduct = miss[HNil]

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

  implicit val identifier: AllIdentifiers[Identifier] = new AllIdentifiers[Identifier] {
    override def apply(t: Identifier) = Seq(t)
  }
  implicit val style = AllIdentifiers[StringLiteral.Style]
  implicit val literal = AllIdentifiers[Literal]
  implicit val tpeConstructor = AllIdentifiers[shorthandAst.TpeConstructor]
  implicit val valueExp = AllIdentifiers[shorthandAst.ValueExp]
  implicit val constructorApp = AllIdentifiers[shorthandAst.ConstructorApp]
  implicit val propertyValue = AllIdentifiers[shorthandAst.PropertyValue]
  implicit val propertyExp = AllIdentifiers[shorthandAst.PropertyExp]
  implicit val bodyStmt = AllIdentifiers[shorthandAst.BodyStmt]
  implicit val topLevel = AllIdentifiers[shorthandAst.TopLevel]
  implicit val sbFile = AllIdentifiers[shorthandAst.SBFile]
//
  def miss[T]: AllIdentifiers[T] = new MissIdentifiers[T]

  class MissIdentifiers[T] extends AllIdentifiers[T] {
    override def apply(t: T) = Seq.empty
  }
}





