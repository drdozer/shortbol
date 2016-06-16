package uk.co.turingatemyhamster
package shortbol.ops

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}
import shortbol.ast._
import shortbol.shapeless._
import uk.co.turingatemyhamster.shortbol.ops.EvalEval.Aux

import scalaz.Scalaz._
import scalaz._

case class EvalContext(rslvr: Resolver,
                       rctxt: ResolutionContext = ResolutionContext(None, PrefixBindingsImpl(None, Map.empty)),
                       cstrs: Map[Identifier, TopLevel.ConstructorDef] = Map.empty,
                       bndgs: Map[Identifier, ValueExp] = Map.empty,
                       thrwn: Seq[Throwable] = Seq.empty)
{

  def withConstructor(c: TopLevel.ConstructorDef) =
    copy(cstrs = cstrs + (c.id -> c))

  def withContext(assignment: Assignment) =
    copy(bndgs = bndgs + (assignment.property -> assignment.value))

  def withContext(names: Seq[Identifier], values: Seq[ValueExp]) =
    copy(bndgs = bndgs ++ (names zip values))

  def resolveBinding(id: Identifier): Option[ValueExp] =
    (bndgs get id) orElse {
      id match {
        case LocalName(name) =>
          (for {
            (QName(_, LocalName(qln)), ve) <- bndgs if qln == name
          } yield ve).headOption // fixme: should report clashes
        case _ => None
      }
    }
}

trait Eval[T] {
  type Result
  def apply(t: T): Eval.EvalState[Result]
}

object EvalEval {
  type Aux[T, R] = Eval[T] { type Result = R }
}


object Eval extends TypeClassCompanion2[EvalEval.Aux] {

  type EvalState[R] = State[EvalContext, R]

  def constant[T](t: T): State[EvalContext, T] = t.point[({type l[a] = State[EvalContext, a]})#l]

//  def noopEval[T, U] = new Eval[T] {
//    override type Result = Option[U]
//    override def apply(t: T) = constant(None)
//  }

  def identityEval[T] = new Eval[T] {
    override type Result = T
    override def apply(t: T) = constant(t)
  }

  object typeClass extends TypeClass2[EvalEval.Aux] {
    override def coproduct[L, R <: Coproduct, LL, RR <: Coproduct](cl: => EvalEval.Aux[L, LL],
                                                                   cr: => EvalEval.Aux[R, RR]) = new Eval[L:+:R]
    {
      override type Result = LL :+: RR

      override def apply(t: L:+:R) = t match {
              case Inl(l) => for (el <- cl apply l) yield Inl(el)
              case Inr(r) => for (er <- cr apply r) yield Inr(er)
            }
    }

    override val emptyCoproduct = identityEval[CNil]

    override val emptyProduct = identityEval[HNil]

    override def product[H, HH, T <: HList, TT <: HList](ch: Aux[H, HH], ct: Aux[T, TT]) = new Eval[H::T]
    {
      override type Result = HH::TT
      override def apply(ht: H::T) = for {
        eh <- ch(ht.head)
        et <- ct(ht.tail)
      } yield eh::et
    }

    override def project[F, G, FF, GG](instance: => EvalEval.Aux[G, GG],
                               to: (F) => G,
                               from: (GG) => FF) = new Eval[F] {
      override type Result = FF
      override def apply(t: F) = for {
        u <- instance(to(t))
      } yield from(u)
    }
  }

  implicit class EvalOps[T](val _t: T) extends AnyVal {
    def eval[U](implicit e: EvalEval.Aux[T, U]): EvalState[U] = e(_t)
  }

  implicit def seq[T, U](implicit pa: EvalEval.Aux[T, U]): EvalEval.Aux[Seq[T], Seq[U]] =
    Eval.typeClass.project(implicitly[EvalEval.Aux[List[T], List[U]]], (_: Seq[T]).to[List], implicitly[List[U]<:<Seq[U]])


  // Smelly! Find a way to compute this
  implicit lazy val topLevel: EvalEval.Aux[TopLevel, Option[TopLevel.InstanceExp]] = {
    type U = Option[TopLevel.InstanceExp]:+:Option[TopLevel.InstanceExp]:+:Option[TopLevel.InstanceExp]:+:Option[TopLevel.InstanceExp]:+:Option[TopLevel.InstanceExp]:+:Option[TopLevel.InstanceExp]:+:CNil
    val g = Generic[TopLevel]
    val e = Eval[g.Repr, U]
    typeClass.project[TopLevel, g.Repr, Option[TopLevel.InstanceExp], U](e, g.to, _.unify)
  }

  implicit val bodyStmt = Eval[BodyStmt, BodyStmt]

  // Smelly! Find a way to conpute this
  implicit lazy val valueExp: EvalEval.Aux[ValueExp, ValueExp] = {
    type U = IntegerLiteral :+: Identifier :+: MultiLineLiteral :+: Identifier :+: StringLiteral :+: Identifier :+: CNil
    val g = Generic[ValueExp]
    val e = Eval[g.Repr, U]
    typeClass.project[ValueExp, g.Repr, ValueExp, U](e, g.to, _.unify)
  }

  implicit val tpeConstructor = Eval[TpeConstructor, TpeConstructor]

  implicit val literal = Eval[Literal, Literal]

  // fixme: see if this is redundant
  implicit val sbFile: EvalEval.Aux[SBFile, Seq[TopLevel.InstanceExp]] = new Eval[SBFile] {
    override type Result = Seq[TopLevel.InstanceExp]

    override def apply(t: SBFile) = for {
      ts <- t.tops.eval
    } yield ts.flatten
  }

  implicit val blankLine: EvalEval.Aux[BlankLine.type, BlankLine.type] = identityEval

  implicit val topLevel_blankLine: EvalEval.Aux[TopLevel.BlankLine, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.BlankLine] {
    override type Result = Option[TopLevel.InstanceExp]

    override def apply(t: TopLevel.BlankLine) = for {
      te <- t.blankLine.eval
    } yield None
  }

  implicit val comment: EvalEval.Aux[Comment, Comment] = identityEval

  implicit val topLevel_comment: EvalEval.Aux[TopLevel.Comment, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.Comment] {
      override type Result = Option[TopLevel.InstanceExp]

      override def apply(t: TopLevel.Comment) = for {
        te <- t.comment.eval
      } yield None
    }

  implicit val `import`: EvalEval.Aux[TopLevel.Import, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.Import] {
    override type Result = Option[TopLevel.InstanceExp]
    override def apply(t: TopLevel.Import) = t match {
      case TopLevel.Import(path) =>
        for {
          resolver <- gets ((_: EvalContext).rslvr)
          ctxt <- gets ((_: EvalContext).rctxt)
          i <- resolver.resolve(ctxt, path) match {
            case \/-(imported) =>
              for {
                ex <- imported.eval
              } yield None
            case -\/(err) =>
              for {
                _ <- modify((ec: EvalContext) => ec.copy(thrwn = ec.thrwn :+ err))
              } yield None
          }
        } yield i
    }
  }

  implicit val constructorDef: EvalEval.Aux[TopLevel.ConstructorDef, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.ConstructorDef] {
    override type Result = Option[TopLevel.InstanceExp]

    override def apply(t: TopLevel.ConstructorDef) = for {
      _ <- modify((_: EvalContext) withConstructor t)
    } yield None
  }

  implicit val assignment: EvalEval.Aux[Assignment, Assignment] = new Eval[Assignment] {
    override type Result = Assignment

    override def apply(t: Assignment) = for {
      p <- t.property.eval
      v <- t.value.eval
    } yield Assignment(p, v)
  }

  implicit val topLevel_assignment: EvalEval.Aux[TopLevel.Assignment, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.Assignment] {
    override type Result = Option[TopLevel.InstanceExp]

    override def apply(t: TopLevel.Assignment) = for {
      a <- t.assignment.eval
      _ <- modify((_: EvalContext).withContext(a))
    } yield None
  }

  implicit val constructorApp: EvalEval.Aux[ConstructorApp, ConstructorApp] = new Eval[ConstructorApp] {
    override type Result = ConstructorApp

    override def apply(ca: ConstructorApp) = {
      ca.cstr match {
        case TpeConstructor1(id, args) =>
          for {
            e <- expandIfNeeded(ca, id, args)
            bs <- withStack(Seq(), Seq())(ca.body.eval[Seq[BodyStmt]])
          } yield {
            e.copy(body = e.body ++ bs)
          }
      }
    }

    def expandIfNeeded(t: ConstructorApp, ident: Identifier, args: Seq[ValueExp]): EvalState[ConstructorApp] = for {
      co <- cstr(ident)
      e <- co match {
        case Some(c) =>
          expandDefinitely(c, args)
        case None =>
          for {
            r <- resolveBinding(ident)
            re <- r match {
              case Some(id : Identifier) =>
                expandIfNeeded(t, id, args)
              case None =>
                constant(t.copy(cstr = rename(t.cstr, ident), body = Seq()))
            }
          } yield re
      }
    } yield e

    def rename(t: TpeConstructor, id: Identifier): TpeConstructor = t match {
      case TpeConstructor1(_, args) =>
        TpeConstructor1(id, args)
      case TpeConstructorStar =>
        TpeConstructorStar
    }

    def expandDefinitely(c: TopLevel.ConstructorDef, args: Seq[ValueExp]) =
      for {
        bdy <- withStack(c.args, args)(c.cstrApp.body.eval)
        cd <- (c.cstrApp.cstr match {
          case TpeConstructor1(ident, _) => ConstructorApp(TpeConstructor1(ident, Seq()), bdy)
        }).eval
      } yield cd

    def withStack[T](names: Seq[LocalName], values: Seq[ValueExp])(sf: EvalState[T]) = for {
      ec <- get[EvalContext]
      _ <- modify ((_: EvalContext).withContext(names, values))
      v <- sf
      _ <- put(ec) // fixme: we should be only overwriting the bindings
    } yield v
  }


  implicit val instanceExp: EvalEval.Aux[InstanceExp, Option[InstanceExp]] = new Eval[InstanceExp] {
    override type Result = Option[InstanceExp]

    override def apply(i: InstanceExp) = for {
      ce <- i.cstrApp.eval
    } yield Some(InstanceExp(i.id, ce))
  }

  implicit val topLevel_instanceExp: EvalEval.Aux[TopLevel.InstanceExp, Option[TopLevel.InstanceExp]] = new Eval[TopLevel.InstanceExp] {
    override type Result = Option[TopLevel.InstanceExp]

    override def apply(t: TopLevel.InstanceExp) = for {
      oi <- t.instanceExp.eval
    } yield oi map TopLevel.InstanceExp
  }

  implicit val tpeConstructor1: EvalEval.Aux[TpeConstructor1, TpeConstructor1] = new Eval[TpeConstructor1] {
    override type Result = TpeConstructor1

    override def apply(t: TpeConstructor1) = for {
      id <- t.id.eval
      args <- t.args.eval[Seq[ValueExp]] // fixme: unable to derive U =:= Seq[ValueExp]
    } yield TpeConstructor1(id, args)
  }

  implicit val tpeConstructorStar: EvalEval.Aux[TpeConstructorStar.type, TpeConstructorStar.type] = identityEval

  implicit val identifier: EvalEval.Aux[Identifier, Identifier] = new Eval[Identifier] {
    override type Result = Identifier

    def apply(id: Identifier) = resolveWithAssignment(id)

    def resolveWithAssignment(id: Identifier): State[EvalContext, Identifier] = for {
      b <- resolveBinding(id)
      rb <- b match {
        case Some(rid : Identifier) =>
          resolveWithAssignment(rid)
        case _ =>
          constant(id)
      }
    } yield rb
  }

  def as[T, U](implicit e: EvalEval.Aux[U, U], to: T <:< U): EvalEval.Aux[T, U] =
    typeClass.project[T, U, U, U](e, to, identity)

  implicit val stringLiteral: EvalEval.Aux[StringLiteral, StringLiteral] = identityEval
  implicit val multiLineLiteral: EvalEval.Aux[MultiLineLiteral, MultiLineLiteral] = identityEval
  implicit val integerLiteral: EvalEval.Aux[IntegerLiteral, IntegerLiteral] = identityEval
//  implicit val localName: EvalEval.Aux[LocalName, LocalName] = identityEval
//  implicit val qname: EvalEval.Aux[QName, QName] = identityEval
//  implicit val url: EvalEval.Aux[Url, Url] = identityEval
  implicit val localName: EvalEval.Aux[LocalName, Identifier] = as[LocalName, Identifier]
  implicit val qname: EvalEval.Aux[QName, Identifier] = as[QName, Identifier]
  implicit val url: EvalEval.Aux[Url, Identifier] = as[Url, Identifier]

  def cstr(id: Identifier): State[EvalContext, Option[TopLevel.ConstructorDef]] =
    gets ((_: EvalContext).cstrs.get(id))

  def resolveBinding(id: Identifier): State[EvalContext, Option[ValueExp]] =
    gets ((_: EvalContext).resolveBinding(id))

}
