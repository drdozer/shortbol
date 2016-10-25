package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.shorthandAst
import uk.co.turingatemyhamster.shortbol.shorthandAst.AstNode

trait AllNodes[T] {
  def apply(t: T): Seq[shorthandAst.AstNode]
}

object AllNodes extends TypeClassCompanion[AllNodes] {
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

  implicit def seq[T](implicit e: AllNodes[T]): AllNodes[Seq[T]] = new AllNodes[Seq[T]] {
    override def apply(t: Seq[T]) = t flatMap e.apply
  }

  implicit val missString = miss[String]
  implicit val missInt = miss[Int]
  implicit val missBoolean = miss[Boolean]

  implicit val localName = AllNodes[shorthandAst.LocalName]
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
  implicit val assignment = AllNodes[shorthandAst.Assignment]
  implicit val blankLine = AllNodes[shorthandAst.BlankLine]
  implicit val comment = AllNodes[shorthandAst.Comment]
  implicit val constructorApp = AllNodes[shorthandAst.ConstructorApp]
  implicit val constructorDef = AllNodes[shorthandAst.ConstructorDef]
  implicit val instanceExp = AllNodes[shorthandAst.InstanceExp]
  implicit val pragma = AllNodes[shorthandAst.Pragma]
  implicit val bodyStmt = AllNodes[shorthandAst.BodyStmt]
  implicit val topLevel = AllNodes[shorthandAst.TopLevel]
  implicit val sbFile = AllNodes[shorthandAst.SBFile]

  def miss[T]: MissNode[T] = new MissNode[T]
  class MissNode[T] extends AllNodes[T] {
    override def apply(t: T): Seq[AstNode] = Seq.empty
  }

  def in[T](t: T)(implicit e: AllNodes[T]) = e(t)
}