package uk.co.turingatemyhamster.shortbol

import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, TypeClass, TypeClassCompanion}
import uk.co.turingatemyhamster.shortbol.Eval.EvalState

import scalaz._
import Scalaz._

case class EvalContext(rslvr: Resolver,
                       rctxt: ResolutionContext = ResolutionContext(None, PrefixBindingsImpl(None, Map.empty)),
                       cstrs: Map[Identifier, ConstructorDef] = Map.empty,
                       bndgs: Map[Identifier, ValueExp] = Map.empty,
                       thrwn: Seq[Throwable] = Seq.empty)
{

  def withConstructor(c: ConstructorDef) =
    copy(cstrs = cstrs + (c.id -> c))

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
  //self =>

  type Result

  def apply(t: T): EvalState[Result]
//  final def log(msg: String) = new Eval[T] {
//    override def apply(t: T): EvalState[T] = for {
//      e <- self.apply(t)
//    } yield {
//        e
//      }
//  }
}


object Eval extends TypeClassCompanion[Eval] {
  type Aux[T, R] = Eval[T] { type Result = R }

  type EvalState[R] = State[EvalContext, R]

  object typeClass extends TypeClass[Eval] {
    override def coproduct[L, R <: Coproduct](cl: => Eval.Aux[L, L],
                                              cr: => Eval.Aux[R, R]) = new Eval[L:+:R] {
      override type Result = L:+:R

      override def apply(t: L:+:R) = t match {
              case Inl(l) => for (el <- cl apply l) yield Inl(el)
              case Inr(r) => for (er <- cr apply r) yield Inr(er)
            }
    }

    object emptyCoproduct extends Eval[CNil] {
      override type Result = CNil
      override def apply(t: CNil) = constant(t)
    }

    object emptyProduct extends Eval[HNil] {
      override type Result = HNil
      override def apply(t: HNil) = constant(t)
    }

    override def product[H, T <: HList](ch: Eval.Aux[H, H],
                                        ct: Eval.Aux[T, T]) = new Eval[H::T] {
      override type Result = H::T
      override def apply(ht: H::T) = for {
        eh <- ch(ht.head)
        et <- ct(ht.tail)
      } yield eh::et
    }

    override def project[F, G](instance: => Eval.Aux[G, G],
                               to: (F) => G,
                               from: (G) => F) = new Eval[F] {
      override type Result = F
      override def apply(t: F) = for {
        u <- instance(to(t))
      } yield from(u)
    }
  }

  implicit class EvalOps[T](val _t: T) extends AnyVal {
    def eval(implicit e: Eval[T]): EvalState[e.Result] = e(_t)
  }

  implicit def seqExpander[T, R](implicit evT: Eval.Aux[T, R]): Eval[List[T]] = new Eval[List[T]] {
    override type Result = List[R]
    override def apply(ts: List[T]) = ts.traverseS(_.eval)
  }

  // fixme: see if this is redundant
  implicit object SBFileExpander extends Eval[SBFile] {
    override type Result = List[InstanceExp]
    override def apply(t: SBFile) = t.tops.eval(seqExpander[TopLevel, InstanceExp](TopLevelExpander))
  }

  implicit object TopLevelExpander extends Eval[TopLevel] {
    override type Result = InstanceExp
    override def apply(t: TopLevel) = ???
  }

  //
//  def singleton[T](t: T): ExState[T] = List(t).point[({type l[a] = State[ExpansionContext, a]})#l]
//
  def constant[T](t: T): State[EvalContext, T] = t.point[({type l[a] = State[EvalContext, a]})#l]
//
//  def noopExpander[T]: Expander[T, T] = new Expander[T, T] {
//    override def apply(t: T): ExState[T] =
//      singleton(t)
//  }
//
//  def swallowExpander[T]: Expander[T, T] = new Expander[T, T] {
//    override def apply(t: T): ExState[T] =
//      List.empty.point[({type l[a] = State[ExpansionContext, a]})#l]
//  }
//
//  implicit val BlankLineExpander: Expander[BlankLine.type, BlankLine.type] = swallowExpander[BlankLine.type]
//  implicit val CommentExpander: Expander[Comment, Comment] = swallowExpander[Comment]
//
//  implicit val ImportExpander: Expander[Ast.SBOL.Import, Ast.SBOL.Import] = new Expander[Ast.SBOL.Import, Ast.SBOL.Import] {
//    override def apply(t: Ast.SBOL.Import): ExState[Ast.SBOL.Import] = t match {
//      case UnprocessedImport(path) =>
//        for {
//          resolver <- gets ((_: ExpansionContext).rslvr)
//          ctxt <- gets ((_: ExpansionContext).rctxt)
//          i <- resolver.resolve(ctxt, path) match {
//            case \/-(imported) =>
//              for {
//                ex <- imported.expansion
//              } yield ProcessedImport(path, SBFile(ex.map(_.tops).flatten)) :: Nil
//            case -\/(err) =>
//              for {
//                _ <- modify((ec: ExpansionContext) => ec.copy(thrwn = ec.thrwn :+ err))
//              } yield t :: Nil
//          }
//        } yield i
//    }
//  }
//
//  implicit val ConstructorDefExpander: Expander[Ast.SBOL.ConstructorDef, Ast.SBOL.ConstructorDef] = new Expander[Ast.SBOL.ConstructorDef, Ast.SBOL.ConstructorDef] {
//    override def apply(t: Ast.SBOL.ConstructorDef) = for {
//      _ <- iModify((_: ExpansionContext) withConstructor t)
//    } yield Nil
//  }
//
//  implicit val AssignmentExpander: Expander[Ast.SBOL.Assignment, Ast.SBOL.Assignment] = new Expander[Ast.SBOL.Assignment, Ast.SBOL.Assignment] {
//    override def apply(t: Assignment): ExState[Assignment] = for {
//      ps <- t.property.expansion
//      vs <- t.value.expansion
//      as = for {
//        p <- ps
//        v <- vs
//      } yield Assignment(p, v)
//      _ <- modify((_: ExpansionContext).withContext(ps, vs))
//    } yield as
//  }
//
//  /**
//    * Returns list of top levels
//    * Implicit - So I assume that scala figures out which expansion to call.
//    */
//  implicit val TopLevelExpander: Expander[Ast.SBOL.TopLevel, Ast.SBOL.TopLevel] = new Expander[Ast.SBOL.TopLevel, Ast.SBOL.TopLevel] {
//    override def apply(t: Ast.SBOL.TopLevel): ExState[Ast.SBOL.TopLevel] = t match {
//      case BlankLine =>
//        for { e <- BlankLine.expansion } yield e
//      case i : Ast.SBOL.Import =>
//        for { e <- i.expansion } yield e
//      case a : Assignment =>
//        for { e <- a.expansion } yield e
//      case c : Comment =>
//        for { e <- c.expansion } yield e
//      case i : Ast.SBOL.InstanceExp =>
//        for { e <- i.expansion } yield e
//      case c : Ast.SBOL.ConstructorDef =>
//        for { e <- c.expansion } yield e
//    }
//  }
//
//  implicit def SeqExpander[T, U](implicit e: Expander[T, U]): Expander[Seq[T], Seq[U]] = new Expander[Seq[T], Seq[U]] {
//    override def apply(ts: Seq[T]): ExState[Seq[U]] =
//      for {
//        es <- ts.to[List].traverseS(b => b.expansion)
//      } yield es
//  }
//
//  implicit val ConstructorAppExpander: Expander[Ast.SBOL.ConstructorApp, Ast.SBOL.ConstructorApp] = new Expander[Ast.SBOL.ConstructorApp, Ast.SBOL.ConstructorApp] {
//
//    override def apply(ca: Ast.SBOL.ConstructorApp): ExState[Ast.SBOL.ConstructorApp] = {
//      object atCstr extends Poly1 {
//        implicit val atTpeConstructor1 = at[Ast.SBOL.TpeConstructor1] { t =>
//          for {
//            es <- expandIfNeeded(ca, t.id, t.args)
//            bs <- withStack(Seq(), Seq())(ca.body.expansion)
//          } yield for {
//            e <- es
//          } yield {
//            e.copy(body = e.body ++ bs.flatten)
//          }
//        }
//      }
//
//      ca.cstr map atCstr
//    }
//
//    def expandIfNeeded(t: Ast.SBOL.ConstructorApp, ident: Ast.SBOL.Identifier, args: Seq[Ast.SBOL.ValueExp]): ExState[Ast.SBOL.ConstructorApp] = for {
//      co <- cstr(ident)
//      e <- co match {
//        case Some(c) =>
//          expandDefinitely(c, args)
//        case None =>
//          for {
//            r <- resolveBinding(ident)
//            re <- r match {
//              case Some(id : Ast.SBOL.Identifier) =>
//                expandIfNeeded(t, id, args)
//              case None =>
//                singleton(t.copy(cstr = rename(t.cstr, ident), body = Seq()))
//            }
//          } yield re
//      }
//    } yield e
//
//    def rename(t: Ast.SBOL.TpeConstructor, id: Ast.SBOL.Identifier): Ast.SBOL.TpeConstructor = {
//      object renamer extends Poly1 {
//        at[Ast.SBOL.TpeConstructor1](t1 => t1.copy(id = id))
//        at[TpeConstructorStar.type](ts => ts)
//      }
//
//      t map renamer
//    }
//
//    def expandDefinitely(c: Ast.SBOL.ConstructorDef, args: Seq[Ast.SBOL.ValueExp]): ExState[Ast.SBOL.ConstructorApp] =
//      for {
//        bdy <- withStack(c.args, args)(c.cstrApp.body.expansion)
//        cd <- (c.cstrApp.cstr match {
//          case TpeConstructor1(ident, _) => Ast.SBOL.ConstructorApp(TpeConstructor1(ident, Seq()), bdy.flatten)
//        }).expansion
//      } yield cd
//
//    def withStack[T](names: Seq[LocalName], values: Seq[Ast.SBOL.ValueExp])(sf: ExState[T]): ExState[T] = for {
//      ec <- get[ExpansionContext]
//      _ <- modify ((_: ExpansionContext).withContext(names, values))
//      v <- sf
//      _ <- put(ec) // fixme: we should be only overwriting the bindings
//    } yield v
//  }
//
//  implicit val InstanceExpExpander: Expander[Ast.SBOL.InstanceExp, Ast.SBOL.InstanceExp] = new Expander[Ast.SBOL.InstanceExp, Ast.SBOL.InstanceExp] {
//    def apply(i: Ast.SBOL.InstanceExp) = {
//      for {
//        ce <- i.cstrApp.expansion
//      } yield for { c <- ce } yield InstanceExp(i.id, c)
//    }
//  }
//
//  implicit val BodyStmtExpander: Expander[Ast.SBOL.BodyStmt, Ast.SBOL.BodyStmt] = new Expander[Ast.SBOL.BodyStmt, Ast.SBOL.BodyStmt] {
//    def apply(stmt: BodyStmt): ExState[BodyStmt] = stmt match {
//      case Assignment(prop, value) =>
//        for {
//          pe <- prop.expansion
//          ve <- value.expansion
//        } yield for { p <- pe; v <- ve } yield Assignment(p, v)
//      case c : Ast.SBOL.ConstructorApp =>
//        for { e <- c.expansion } yield e
//      case Ast.SBOL.NestedInstance(nested) =>
//        for {
//          ie <- nested.expansion
//        } yield for { i <- ie } yield NestedInstance(i)
//      case BlankLine =>
//        for { e <- BlankLine.expansion } yield e
//      case c : Comment =>
//        for { e <- c.expansion } yield e
//    }
//  }
//
//  implicit val IdentifierExpander: Expander[Ast.SBOL.Identifier, Ast.SBOL.Identifier] = new Expander[Ast.SBOL.Identifier, Ast.SBOL.Identifier] {
//    def apply(id: Ast.SBOL.Identifier) = for {
//      r <- resolveWithAssignment(id)
//    } yield r :: Nil
//
//    def resolveWithAssignment(id: Ast.SBOL.Identifier): State[ExpansionContext, Ast.SBOL.Identifier] = for {
//      b <- resolveBinding(id)
//      rb <- b match {
//        case Some(rid : Ast.SBOL.Identifier) =>
//          resolveWithAssignment(rid)
//        case _ =>
//          constant(id)
//      }
//    } yield rb
//  }
//
//  implicit val ValueExpExpander: Expander[Ast.SBOL.ValueExp, Ast.SBOL.ValueExp] = new Expander[Ast.SBOL.ValueExp, Ast.SBOL.ValueExp] {
//    def apply(ve: Ast.SBOL.ValueExp): ExState[Ast.SBOL.ValueExp] = ve match {
//      case id: Ast.SBOL.Identifier =>
//        for {
//          e <- id.expansion
//        } yield e
//      case _ =>
//        singleton(ve)
//    }
//  }
//
//  def cstr(id: Ast.SBOL.Identifier): State[ExpansionContext, Option[ConstructorDef]] =
//    gets ((_: ExpansionContext).cstrs.get(id))
//
//  def resolveBinding(id: Ast.SBOL.Identifier): State[ExpansionContext, Option[Ast.SBOL.ValueExp]] =
//    gets ((_: ExpansionContext).resolveBinding(id))

}
