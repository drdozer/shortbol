package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.shorthandAst
import uk.co.turingatemyhamster.shortbol.shorthandAst.AstNode
import uk.co.turingatemyhamster.shortbol.longhandAst

trait AllNodes[T] {
  def apply(t: T): Seq[shorthandAst.AstNode]
}

object AllNodes {
  private object typeclassFactory extends TypeClassCompanion[AllNodes] {
    override val typeClass: TypeClass[AllNodes] = new TypeClass[AllNodes] {
      override def coproduct[L, R <: Coproduct](cl: => AllNodes[L], cr: => AllNodes[R]): AllNodes[:+:[L, R]] = new AllNodes[:+:[L, R]] {
        override def apply(t: :+:[L, R]): Seq[AstNode] = t match {
          case Inl(l) => cl(l)
          case Inr(r) => cr(r)
        }
      }

      override def emptyCoproduct: AllNodes[CNil] = new AllNodes[CNil] {
        override def apply(t: CNil): Seq[AstNode] = ???
      }

      override def emptyProduct: AllNodes[HNil] = new AllNodes[HNil] {
        override def apply(t: HNil): Seq[AstNode] = Seq.empty
      }

      override def product[H, T <: HList](ch: AllNodes[H], ct: AllNodes[T]): AllNodes[::[H, T]] = new AllNodes[::[H, T]] {
        override def apply(t: ::[H, T]): Seq[AstNode] = ch(t.head) ++ ct(t.tail)
      }

      override def project[F, G](instance: => AllNodes[G], to: (F) => G, from: (G) => F): AllNodes[F] = new AllNodes[F] {
        override def apply(t: F): Seq[AstNode] = instance(to(t))
      }
    }

    implicit def deriveInstanceH[F, G <: HList](implicit gen: Generic.Aux[F, G], cg: Lazy[AllNodes[G]]): AllNodes[F] = {
      val delegate = deriveInstance[F, G]
      new AllNodes[F] {
        override def apply(t: F): Seq[AstNode] = (t match {
          case a: AstNode => Seq(a)
          case _ => Seq.empty
        }) ++ delegate(t)
      }
    }

    def miss[T]: MissNode[T] = new MissNode[T]
    class MissNode[T] extends AllNodes[T] {
      override def apply(t: T): Seq[AstNode] = Seq.empty
    }
  }

  def apply[T](implicit e: AllNodes[T]): AllNodes[T] = e

  import typeclassFactory._

  implicit val missString: AllNodes[String] = miss[String]
  implicit val missInt: AllNodes[Int] = miss[Int]
  implicit val missBoolean: AllNodes[Boolean] = miss[Boolean]

  implicit val localName = AllNodes[shorthandAst.LocalName]
  implicit val nsPrefix = AllNodes[shorthandAst.NSPrefix]
  implicit val qname = AllNodes[shorthandAst.QName]
  implicit val url = AllNodes[shorthandAst.Url]
  implicit val identifier = AllNodes[shorthandAst.Identifier]
  implicit val singleLine = AllNodes[shorthandAst.StringLiteral.SingleLine]
  implicit val multiLine = AllNodes[shorthandAst.StringLiteral.MultiLine]
  implicit val style = AllNodes[shorthandAst.StringLiteral.Style]
  implicit val datatype = AllNodes[shorthandAst.Datatype]
  implicit val language = AllNodes[shorthandAst.Language]
  implicit val stringLiteral = AllNodes[shorthandAst.StringLiteral]
  implicit val integerLiteral = AllNodes[shorthandAst.IntegerLiteral]
  implicit val literal = AllNodes[shorthandAst.Literal]
  implicit val valueExp = AllNodes[shorthandAst.ValueExp]
  implicit val tpeConstructor1 = AllNodes[shorthandAst.TpeConstructor1]
  implicit val tpeConstructorStar = AllNodes[shorthandAst.TpeConstructorStar]
  implicit val tpeConstructor = AllNodes[shorthandAst.TpeConstructor]
  implicit val l_tpeConstructor = AllNodes[longhandAst.TpeConstructor]
  implicit val assignment = AllNodes[shorthandAst.Assignment]
  implicit val blankLine = AllNodes[shorthandAst.BlankLine]
  implicit val comment = AllNodes[shorthandAst.Comment]
  implicit val constructorApp = AllNodes[shorthandAst.ConstructorApp]
  implicit val l_constructorApp = AllNodes[longhandAst.ConstructorApp]
  implicit val constructorDef = AllNodes[shorthandAst.ConstructorDef]
  implicit val instanceExp = AllNodes[shorthandAst.InstanceExp]
  implicit val l_instanceExp = AllNodes[longhandAst.InstanceExp]
  implicit val pragma = AllNodes[shorthandAst.Pragma]
  implicit val l_propertyExp = AllNodes[longhandAst.PropertyExp]
  implicit val bodyStmt_propertyExp = AllNodes[shorthandAst.BodyStmt.PropertyExp]
  implicit val bodyStmt = AllNodes[shorthandAst.BodyStmt]
  implicit val topLevel_constructorDef = AllNodes[shorthandAst.TopLevel.ConstructorDef]
  implicit val topLevel = AllNodes[shorthandAst.TopLevel]
  implicit val sbFile = AllNodes[shorthandAst.SBFile]
  implicit val l_sbFile = AllNodes[longhandAst.SBFile]

  implicit def fromPair[A, B](implicit a: AllNodes[A], b: AllNodes[B]): AllNodes[(A, B)] = new MissNode[(A, B)] {
    override def apply(t: (A, B)): Seq[AstNode] = a(t._1) ++ b(t._2)
  }

  implicit def list[T](implicit e: AllNodes[T]): AllNodes[List[T]] = new AllNodes[List[T]] {
    override def apply(t: List[T]) = t flatMap e.apply
  }


  def in[T](t: T)(implicit e: AllNodes[T]) = e(t)
}