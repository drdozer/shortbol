package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.shorthandAst

/**
  *
  *
  * @author Matthew Pocock
  */
trait AllQNames[T] {
  def apply(t: T): Seq[shorthandAst.QName]
}

object AllQNames extends TypeClassCompanion[AllQNames] {
  def in[T](t: T)(implicit e: AllQNames[T]) = e(t)

  override val typeClass: TypeClass[AllQNames] = new TypeClass[AllQNames] {
    override def coproduct[L, R <: Coproduct](cl: => AllQNames[L],
                                              cr: => AllQNames[R]) = new AllQNames[:+:[L, R]] {
      override def apply(t: :+:[L, R]) = t match {
        case Inl(l) => cl(l)
        case Inr(r) => cr(r)
      }
    }

    override def emptyCoproduct = new AllQNames[CNil] {
      override def apply(t: CNil) = ???
    }

    override def emptyProduct = miss[HNil]

    override def product[H, T <: HList](ch: AllQNames[H],
                                        ct: AllQNames[T]) = new AllQNames[::[H, T]] {
      override def apply(t: ::[H, T]) = ch(t.head) ++ ct(t.tail)
    }

    override def project[F, G](instance: => AllQNames[G],
                               to: (F) => G,
                               from: (G) => F) = new AllQNames[F] {
      override def apply(t: F) = instance(to(t))
    }
  }

  implicit def seq[T](implicit e: AllQNames[T]): AllQNames[Seq[T]] = new AllQNames[Seq[T]] {
    override def apply(t: Seq[T]) = t flatMap e.apply
  }

  implicit val missString = miss[String]
  implicit val missInt = miss[Int]
  implicit val missBoolean = miss[Boolean]

  implicit val qname: AllQNames[shorthandAst.QName] = new AllQNames[shorthandAst.QName] {
    override def apply(t: shorthandAst.QName) = Seq(t)
  }

  implicit val identifier = AllQNames[shorthandAst.Identifier]
  implicit val style = AllQNames[shorthandAst.StringLiteral.Style]
  implicit val literal = AllQNames[shorthandAst.Literal]
  implicit val tpeConstructor = AllQNames[shorthandAst.TpeConstructor]
  implicit val assignmentSeq = AllQNames[Seq[shorthandAst.Assignment]]
  implicit val instanceExpSeq = AllQNames[Seq[shorthandAst.InstanceExp]]
  implicit val constructorApp = AllQNames[shorthandAst.ConstructorApp]
  implicit val constructorDefSeq = AllQNames[Seq[shorthandAst.ConstructorDef]]
  implicit val valueExp = AllQNames[shorthandAst.ValueExp]
  implicit val propertyValue = AllQNames[shorthandAst.PropertyValue]
  implicit val propertyExp = AllQNames[shorthandAst.PropertyExp]
  implicit val bodyStmt = AllQNames[shorthandAst.BodyStmt]
  implicit val topLevel = AllQNames[shorthandAst.TopLevel]
  implicit val sbFile = AllQNames[shorthandAst.SBFile]

  def miss[T]: AllQNames[T] = new MissQName[T]

  class MissQName[T] extends AllQNames[T] {
    override def apply(t: T) = Seq.empty
  }
}