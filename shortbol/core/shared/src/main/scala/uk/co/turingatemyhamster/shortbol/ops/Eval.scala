package uk.co.turingatemyhamster
package shortbol.ops

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr}
import shortbol.ast._
import shortbol.shapeless._

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

    override def product[H, T <: HList, HH, TT <: HList](ch: EvalEval.Aux[H, HH],
                                        ct: EvalEval.Aux[T, TT]) = new Eval[H::T]
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
    def eval(implicit e: Eval[T]): EvalState[e.Result] = e(_t)
  }

//  implicit def seqExpander[T, R](implicit evT: EvalEval.Aux[T, R]): Eval[List[T]] = new Eval[List[T]] {
//    override type Result = List[R]
//    override def apply(ts: List[T]) = ts.traverseS(_.eval)
//  }

  implicit def seq[T, U](implicit pa: EvalEval.Aux[List[T], List[U]]): EvalEval.Aux[Seq[T], Seq[U]] =
    Eval.typeClass.project(pa, (_: Seq[T]).to[List], implicitly[List[U]<:<Seq[U]])

  implicit lazy val tpeConstructor = Eval[TpeConstructor, TpeConstructor]
  implicit lazy val valueExp = Eval[ValueExp, ValueExp]
  implicit lazy val bdyStmt = Eval[BodyStmt, BodyStmt]
  implicit lazy val topLevel = Eval[TopLevel, Option[TopLevel.InstanceExp]]

  // fixme: see if this is redundant
  implicit val sbFile: EvalEval.Aux[SBFile, Seq[TopLevel.InstanceExp]] = new Eval[SBFile] {
    override type Result = Seq[TopLevel.InstanceExp]
    val glt = Generic[List[TopLevel]]
    val glo = Generic[List[Option[TopLevel.InstanceExp]]]
    val eea = Eval[glt.Repr, glo.Repr]
    val eeb = Eval[shapeless.:+:[scala.collection.immutable.::[TopLevel],shapeless.:+:[scala.collection.immutable.Nil.type,shapeless.CNil]],
      shapeless.:+:[scala.collection.immutable.::[Option[TopLevel.InstanceExp]],shapeless.:+:[scala.collection.immutable.Nil.type,shapeless.CNil]]]
    val tl = Eval[TopLevel, Option[TopLevel.InstanceExp]]
    val tlas = Eval[TopLevel.Assignment, Option[TopLevel.InstanceExp]]
    val tlbl = Eval[TopLevel.BlankLine, Option[TopLevel.InstanceExp]]
    val tlim = Eval[TopLevel.Import, Option[TopLevel.InstanceExp]]
    val tlco = Eval[TopLevel.Comment, Option[TopLevel.InstanceExp]]
    val tlin = Eval[TopLevel.InstanceExp, Option[TopLevel.InstanceExp]]
    val tlcd = Eval[TopLevel.ConstructorDef, Option[TopLevel.InstanceExp]]
    override def apply(t: SBFile) = for {
      ts <- t.tops.eval(seq[TopLevel, Option[TopLevel.InstanceExp]](
        deriveInstance[
          List[TopLevel],
          glt.Repr,
          List[Option[TopLevel.InstanceExp]],
          glo.Repr]))
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
            es <- expandIfNeeded(ca, id, args)
            bs <- withStack(Seq(), Seq())(ca.body.eval)
          } yield for {
            e <- es
          } yield {
            e.copy(body = e.body ++ bs.flatten)
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
          case TpeConstructor1(ident, _) => ConstructorApp(TpeConstructor1(ident, Seq()), bdy.flatten)
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

  //
//  implicit val BodyStmtExpander: Expander[Ast.SBOL.BodyStmt, Ast.SBOL.BodyStmt] = new Expander[Ast.SBOL.BodyStmt, Ast.SBOL.BodyStmt] {
//    def apply(stmt: BodyStmt): ExState[BodyStmt] = stmt match {
//      case Assignment(prop, value) =>
//        for {
//          pe <- prop.eval
//          ve <- value.eval
//        } yield for { p <- pe; v <- ve } yield Assignment(p, v)
//      case c : Ast.SBOL.ConstructorApp =>
//        for { e <- c.eval } yield e
//      case Ast.SBOL.NestedInstance(nested) =>
//        for {
//          ie <- nested.eval
//        } yield for { i <- ie } yield NestedInstance(i)
//      case BlankLine =>
//        for { e <- BlankLine.eval } yield e
//      case c : Comment =>
//        for { e <- c.eval } yield e
//    }
//  }
//

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

  implicit val literal: EvalEval.Aux[Literal, Literal] = identityEval

  def cstr(id: Identifier): State[EvalContext, Option[TopLevel.ConstructorDef]] =
    gets ((_: EvalContext).cstrs.get(id))

  def resolveBinding(id: Identifier): State[EvalContext, Option[ValueExp]] =
    gets ((_: EvalContext).resolveBinding(id))

}
