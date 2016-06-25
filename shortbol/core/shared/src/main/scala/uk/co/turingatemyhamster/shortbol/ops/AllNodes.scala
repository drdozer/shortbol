package uk.co.turingatemyhamster.shortbol.ops

import shapeless._
import uk.co.turingatemyhamster.shortbol.ast
import uk.co.turingatemyhamster.shortbol.ast.AstNode

trait AllNodes[T] {
  def apply(t: T): Seq[ast.AstNode]
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

  implicit val localName = AllNodes[ast.LocalName]
  implicit val qname = AllNodes[ast.QName]
  implicit val url = AllNodes[ast.Url]
  implicit val identifier = AllNodes[ast.Identifier]
  implicit val singleLine = AllNodes[ast.StringLiteral.SingleLine]
  implicit val multiLine = AllNodes[ast.StringLiteral.MultiLine]
  implicit val style = AllNodes[ast.StringLiteral.Style]
  implicit val datatype = AllNodes[ast.Datatype]
  implicit val language = AllNodes[ast.Language]
  implicit val stringLiteral = AllNodes[ast.StringLiteral]
  implicit val integerLiteral = AllNodes[ast.IntegerLiteral]
  implicit val literal = AllNodes[ast.Literal]
  implicit val valueExp = AllNodes[ast.ValueExp]
  implicit val tpeConstructor1 = AllNodes[ast.TpeConstructor1]
  implicit val tpeConstructorStar = AllNodes[ast.TpeConstructorStar]
  implicit val tpeConstructor = AllNodes[ast.TpeConstructor]
  implicit val assignment = AllNodes[ast.Assignment]
  implicit val blankLine = AllNodes[ast.BlankLine]
  implicit val comment = AllNodes[ast.Comment]
  implicit val constructorApp = AllNodes[ast.ConstructorApp]
  implicit val constructorDef = AllNodes[ast.ConstructorDef]
  implicit val instanceExp = AllNodes[ast.InstanceExp]
  implicit val pragma = AllNodes[ast.Pragma]
  implicit val bodyStmt = AllNodes[ast.BodyStmt]
  implicit val topLevel = AllNodes[ast.TopLevel]
  implicit val sbFile = AllNodes[ast.SBFile]

  def miss[T]: MissNode[T] = new MissNode[T]
  class MissNode[T] extends AllNodes[T] {
    override def apply(t: T): Seq[AstNode] = Seq.empty
  }

  def in[T](t: T)(implicit e: AllNodes[T]) = e(t)
}