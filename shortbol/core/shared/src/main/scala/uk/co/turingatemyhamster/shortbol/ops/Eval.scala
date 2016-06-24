package uk.co.turingatemyhamster
package shortbol.ops

import shortbol.ast._

sealed trait LogLevel

object LogLevel {
  object Info extends LogLevel
  object Warning extends LogLevel
  object Error extends LogLevel
}

case class LogMessage(msg: String, level: LogLevel, cause: Option[Throwable])

object LogMessage {
  def info(msg: String, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Info, cause)
  def warning(msg: String, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Warning, cause)
  def error(msg: String, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Error, cause)
}

case class Hooks(phook: Vector[Pragma => Eval.EvalState[List[Pragma]]] = Vector.empty,
                 ihook: Vector[InstanceExp => Eval.EvalState[List[InstanceExp]]] = Vector.empty,
                 chook: Vector[ConstructorDef => Eval.EvalState[List[ConstructorDef]]] = Vector.empty)
{
  def withPHooks(ps: (Pragma => Eval.EvalState[List[Pragma]])*) =
    copy(phook = phook ++ ps)

  def withIHooks(is: (InstanceExp => Eval.EvalState[List[InstanceExp]])*) =
    copy(ihook = ihook ++ is)

  def withCHooks(cs: (ConstructorDef => Eval.EvalState[List[ConstructorDef]])*) =
    copy(chook = chook ++ cs)
}

case class EvalContext(prgms: Map[Identifier, List[Pragma]] = Map.empty,
                       cstrs: Map[Identifier, List[ConstructorDef]] = Map.empty,
                       vlxps: Map[Identifier, List[ValueExp]] = Map.empty,
                       insts: Map[Identifier, List[InstanceExp]] = Map.empty,
                       hooks: Hooks = Hooks(),
                       logms: Seq[LogMessage] = Seq.empty)
{

  def withConstructors(cs: ConstructorDef*) =
    copy(cstrs = cstrs ++ cs.map(c => c.id -> (c :: cstrs.getOrElse(c.id, Nil))))

  def withAssignments(as: Assignment*) =
    copy(vlxps = vlxps ++ as.map(a => a.property -> (a.value :: vlxps.getOrElse(a.property, Nil))))

  def withPragmas(ps: Pragma*) =
    copy(prgms = prgms ++ ps.map(p => p.id -> (p :: prgms.getOrElse(p.id, Nil))))

  def withInstances(is: InstanceExp*) =
    copy(insts = insts ++ is.map(i => i.id -> (i :: insts.getOrElse(i.id, Nil))))

  def withLog(lm: LogMessage*) =
    copy(logms = logms ++ lm)

  def resolveValue(id: Identifier): Option[ValueExp] =
    vlxps get id map (_.head) orElse {
      id match {
        case LocalName(name) =>
          (for {
            (QName(_, LocalName(qln)), ve::_) <- vlxps if qln == name
          } yield ve).headOption // fixme: should report clashes
        case _ => None
      }
    }

  def resolveCstr(id: Identifier): Option[ConstructorDef] =
    cstrs get id map (_.head) orElse {
      id match {
        case LocalName(name) =>
          (for {
            (QName(_, LocalName(qln)), ve::_) <- cstrs if qln == name
          } yield ve).headOption // fixme: should report clashes
        case _ => None
      }
    }

  def resolveInst(id: Identifier): Option[InstanceExp] =
    insts get id map (_.head) orElse {
      id match {
        case LocalName(name) =>
          (for {
            (QName(_, LocalName(qln)), ie::_) <- insts if qln == name
          } yield ie).headOption // fixme: should report clashes
        case _ => None
      }
    }

  def withPHooks(ps: (Pragma => Eval.EvalState[List[Pragma]])*) =
    copy(hooks = hooks.withPHooks(ps :_*))

  def withIHooks(is: (InstanceExp => Eval.EvalState[List[InstanceExp]])*) =
    copy(hooks = hooks.withIHooks(is :_*))

  def withCHooks(cs: (ConstructorDef => Eval.EvalState[List[ConstructorDef]])*) =
    copy(hooks = hooks.withCHooks(cs :_*))
}

sealed trait Eval[T] {
  type Result
  def apply(t: T): Eval.EvalState[Result]
}

object EvalEval {
  type Aux[T, R] = Eval[T] { type Result = R }
}



import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, lens}
import shortbol.shapeless._
import scalaz.Scalaz._
import scalaz._

object Eval extends TypeClassCompanion2[EvalEval.Aux] {
  import EvalEval.Aux

  type EvalState[R] = State[EvalContext, R]

  def log(logMessage: LogMessage) = modify((_: EvalContext).withLog(logMessage))
  def withPHooks(pHook: Pragma => Eval.EvalState[List[Pragma]]) = modify((_: EvalContext).withPHooks(pHook))
  def withIHooks(iHook: InstanceExp => Eval.EvalState[List[InstanceExp]]) = modify((_: EvalContext).withIHooks(iHook))
  def withCHooks(cHook: ConstructorDef => Eval.EvalState[List[ConstructorDef]]) = modify((_: EvalContext).withCHooks(cHook))

  def constant[T](t: T): State[EvalContext, T] = t.point[EvalState]

  def constantEval[T, U](u: U) = new Eval[T] {
    override type Result = U
    override def apply(t: T) = constant(u)
  }

  def identityEval[T] = new Eval[T] {
    override type Result = T
    override def apply(t: T) = constant(t)
  }

  object typeClass extends TypeClass2[Aux] {
    override def coproduct[L, R <: Coproduct, LL, RR <: Coproduct](cl: => Aux[L, LL],
                                                                   cr: => Aux[R, RR]) = new Eval[L:+:R]
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

    override def project[F, G, FF, GG](instance: => Aux[G, GG],
                               to: (F) => G,
                               from: (GG) => FF) = new Eval[F] {
      override type Result = FF
      override def apply(t: F) = for {
        u <- instance(to(t))
      } yield from(u)
    }
  }

  implicit class EvalOps[T](val _t: T) extends AnyVal {
    def eval[U](implicit e: Aux[T, U]): EvalState[U] = e(_t)
  }

  implicit def seq[T, U](implicit pa: Aux[T, U]): Aux[Seq[T], Seq[U]] =
    Eval.typeClass.project(implicitly[Aux[List[T], List[U]]], (_: Seq[T]).to[List], implicitly[List[U]<:<Seq[U]])


  // Smelly! Find a way to compute this
  implicit lazy val topLevel: Aux[TopLevel, List[TopLevel.InstanceExp]] = {
    type U = List[TopLevel.InstanceExp]:+:List[TopLevel.InstanceExp]:+:List[TopLevel.InstanceExp]:+:List[TopLevel.InstanceExp]:+:List[TopLevel.InstanceExp]:+:List[TopLevel.InstanceExp]:+:CNil
    val g = Generic[TopLevel]
    val e = Eval[g.Repr, U]
    typeClass.project[TopLevel, g.Repr, List[TopLevel.InstanceExp], U](e, g.to, _.unify)
  }

  implicit val bodyStmt = Eval[BodyStmt, BodyStmt]

  implicit val literal: Aux[Literal, Literal] = identityEval

  // fixme: see if this is redundant
  implicit val sbFile: Aux[SBFile, Seq[TopLevel.InstanceExp]] = new Eval[SBFile] {
    override type Result = Seq[TopLevel.InstanceExp]

    override def apply(t: SBFile) = for {
      ts <- t.tops.eval
    } yield ts.flatten
  }

  implicit val blankLine: Aux[BlankLine.type, BlankLine.type] = identityEval

  implicit val topLevel_blankLine: Aux[TopLevel.BlankLine, List[TopLevel.InstanceExp]] = new Eval[TopLevel.BlankLine] {
    override type Result = List[TopLevel.InstanceExp]

    override def apply(t: TopLevel.BlankLine) = for {
      te <- t.blankLine.eval
    } yield Nil
  }

  implicit val comment: Aux[Comment, Comment] = identityEval

  implicit val topLevel_comment: Aux[TopLevel.Comment, List[TopLevel.InstanceExp]] = new Eval[TopLevel.Comment] {
      override type Result = List[TopLevel.InstanceExp]

      override def apply(t: TopLevel.Comment) = for {
        te <- t.comment.eval
      } yield Nil
    }

  implicit val topLevel_pragma: Aux[TopLevel.Pragma, List[TopLevel.InstanceExp]] = new Eval[TopLevel.Pragma] {
    override type Result = List[TopLevel.InstanceExp]

    override def apply(t: TopLevel.Pragma) = for {
      p <- t.pragma.eval
      _ <- modify((_: EvalContext).withPragmas(p :_*))
    } yield Nil
  }

  implicit val pragama: Aux[Pragma, List[Pragma]] = new Eval[Pragma] {
    override type Result = List[Pragma]

    override def apply(t: Pragma) = for {
      phook <- gets((_: EvalContext).hooks.phook)
      hook = phook.foldl((p: Pragma) => constant(List(p)))(
        h1 => h2 => (p0: Pragma) => for {
          p1 <- h1(p0)
          p2 <- (p1 map h2).sequence
        } yield p2.flatten)
      cd <- hook(t)
    } yield cd
  }

  implicit val assignment: Aux[Assignment, Assignment] = new Eval[Assignment] {
    override type Result = Assignment

    override def apply(t: Assignment) = for {
      p <- t.property.eval
      v <- t.value.eval
    } yield Assignment(p, v)
  }

  implicit val topLevel_assignment: Aux[TopLevel.Assignment, List[TopLevel.InstanceExp]] = new Eval[TopLevel.Assignment] {
    override type Result = List[TopLevel.InstanceExp]

    override def apply(t: TopLevel.Assignment) = for {
      a <- t.assignment.eval
      _ <- modify((_: EvalContext).withAssignments(a))
    } yield Nil
  }

  implicit val topLevel_constructorDef: Aux[TopLevel.ConstructorDef, List[TopLevel.InstanceExp]] = new Eval[TopLevel.ConstructorDef] {
    override type Result = List[TopLevel.InstanceExp]

    override def apply(t: TopLevel.ConstructorDef) = for {
      cd <- t.constructorDef.eval
      _ <- modify((_: EvalContext).withConstructors(cd :_*))
    } yield Nil
  }

  implicit val constructorDef: Aux[ConstructorDef, List[ConstructorDef]] = new Eval[ConstructorDef] {
    override type Result = List[ConstructorDef]

    override def apply(t: ConstructorDef) = for {
      chook <- gets((_: EvalContext).hooks.chook)
      hook = chook.foldl((c: ConstructorDef) => constant(List(c)))(
        h1 => h2 => (c0: ConstructorDef) => for {
          c1 <- h1(c0)
          c2 <- (c1 map h2).sequence
        } yield c2.flatten)
      cd <- hook(t)
    } yield cd
  }

  implicit val constructorApp: Aux[ConstructorApp, ConstructorApp] = new Eval[ConstructorApp] {
    override type Result = ConstructorApp

    override def apply(ca: ConstructorApp) = for {
      tcTcBody <- ca.cstr.eval
      (tc, tcBody) = tcTcBody
      body <- ca.body.eval
    } yield ConstructorApp(tc, tcBody ++ body)
  }


  implicit val instanceExp: Aux[InstanceExp, List[InstanceExp]] = new Eval[InstanceExp] {
    override type Result = List[InstanceExp]

    override def apply(i: InstanceExp) = for {
      ce <- i.cstrApp.eval
      ihook <- gets((_: EvalContext).hooks.ihook)
      hook = ihook.foldl((i: InstanceExp) => constant(List(i)))(
        h1 => h2 => (i0: InstanceExp) => for
        {
          i1 <- h1(i0)
          i2 <- (i1 map h2).sequence
        } yield i2.flatten)
      is <- hook(InstanceExp(i.id, ce))
    } yield is
  }

  implicit val topLevel_instanceExp: Aux[TopLevel.InstanceExp, List[TopLevel.InstanceExp]] = new Eval[TopLevel.InstanceExp] {
    override type Result = List[TopLevel.InstanceExp]

    override def apply(t: TopLevel.InstanceExp) = for {
      is <- t.instanceExp.eval
      _ <- modify((_: EvalContext).withInstances(is :_*))
    } yield is map TopLevel.InstanceExp
  }

  implicit val constructorDefApp: Aux[(Seq[ValueExp], ConstructorDef), (TpeConstructor, Seq[BodyStmt])] = new Eval[(Seq[ValueExp], ConstructorDef)] {
    override type Result = (TpeConstructor, Seq[BodyStmt])

    override def apply(vscd: (Seq[ValueExp], ConstructorDef)) = for {
      cd <- withStack(vscd._2.args, vscd._1)(vscd._2.cstrApp.eval)
    } yield (cd.cstr, cd.body)

    def withStack[T](names: Seq[LocalName], values: Seq[ValueExp])(sf: EvalState[T]) = for {
      ec <- get[EvalContext]
      _ <- modify ((_: EvalContext).withAssignments(names zip values map (Assignment.apply _).tupled :_*))
      v <- sf
      _ <- put(ec) // fixme: should we only be only overwriting the bindings?
    } yield v
  }

  implicit val tpeConstructor1: Aux[TpeConstructor1, (TpeConstructor, Seq[BodyStmt])] = new Eval[TpeConstructor1] {
    override type Result = (TpeConstructor, Seq[BodyStmt])

    override def apply(t: TpeConstructor1) = for {
      ot <- resolveWithAssignment(t.id)
      args <- t.args.eval
      ts <- ot match {
        case Some(cd) =>
            (args, cd).eval
        case None =>
          constant(t.copy(args = args), Seq.empty)
      }
    } yield ts

    def resolveWithAssignment(id: Identifier): EvalState[Option[ConstructorDef]] = for {
      c <- cstr(id)
      cc <- c match {
        case Some(_) =>
          constant(c)
        case None =>
          for {
            b <- resolveBinding(id)
            bb <- b match {
              case Some(ValueExp.Identifier(nid)) =>
                resolveWithAssignment(nid)
              case _ =>
                constant(None : Option[ConstructorDef])
            }
          } yield bb
      }
    } yield cc
  }

  implicit val tpeConstructorStar: Aux[TpeConstructorStar.type, (TpeConstructorStar.type, Seq[BodyStmt])] =
    constantEval(TpeConstructorStar -> Seq.empty[BodyStmt])

  implicit val tpeConstructor: Aux[TpeConstructor, (TpeConstructor, Seq[BodyStmt])] = {
    type U = (TpeConstructor, Seq[BodyStmt]):+:(TpeConstructorStar.type, Seq[BodyStmt]):+:CNil
    val g = Generic[TpeConstructor]
    val e = Eval[g.Repr, U]
    typeClass.project[TpeConstructor, g.Repr, (TpeConstructor, Seq[BodyStmt]), U](e, g.to, _.unify)
  }

  implicit val identifier: Aux[Identifier, Identifier] = new Eval[Identifier] {
    override type Result = Identifier

    def apply(id: Identifier) = resolveWithAssignment(id)

    def resolveWithAssignment(id: Identifier): State[EvalContext, Identifier] = for {
      b <- resolveBinding(id)
      rb <- b match {
        case Some(ValueExp.Identifier(rid)) =>
          resolveWithAssignment(rid)
        case _ =>
          constant(id)
      }
    } yield rb
  }

  implicit val valueExp: Aux[ValueExp, ValueExp] = new Eval[ValueExp] {
    override type Result = ValueExp

    override def apply(t: ValueExp) = t match {
      case ValueExp.Literal(l) =>
        for {
          ll <- l.eval
        } yield ValueExp.Literal(ll)
      case ValueExp.Identifier(i) =>
        resolveWithAssignment(i)
    }

    def resolveWithAssignment(id: Identifier): State[EvalContext, ValueExp] = for {
      b <- resolveBinding(id)
      rb <- b match {
        case Some(ValueExp.Identifier(rid)) =>
          resolveWithAssignment(rid)
        case Some(l@ValueExp.Literal(_)) =>
          constant(l)
        case None =>
          constant(ValueExp.Identifier(id))
      }
    } yield rb
  }

  def as[T, U](implicit e: Aux[U, U], to: T <:< U): Aux[T, U] =
    typeClass.project[T, U, U, U](e, to, identity)

  implicit val localName: Aux[LocalName, Identifier] = as[LocalName, Identifier]
  implicit val qname: Aux[QName, Identifier] = as[QName, Identifier]
  implicit val url: Aux[Url, Identifier] = as[Url, Identifier]

  def cstr(id: Identifier): State[EvalContext, Option[ConstructorDef]] =
    gets ((_: EvalContext).resolveCstr(id))

  def resolveBinding(id: Identifier): State[EvalContext, Option[ValueExp]] =
    gets ((_: EvalContext).resolveValue(id))

}
