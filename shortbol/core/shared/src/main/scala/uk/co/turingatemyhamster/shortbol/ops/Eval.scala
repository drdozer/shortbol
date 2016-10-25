package uk.co.turingatemyhamster
package shortbol.ops

import shortbol.{shorthandAst => sAst}
import shortbol.{longhandAst => lAst}

sealed trait LogLevel
{
  def pretty: String
}

object LogLevel {
  object Info extends LogLevel    { def pretty = "info" }
  object Warning extends LogLevel { def pretty = "warning" }
  object Error extends LogLevel   { def pretty = "error" }
}

case class LogMessage(msg: String, level: LogLevel, region: sAst.Region, cause: Option[Throwable])
{
  def pretty = s"${level.pretty}: $msg" + cause.map(t => " because " ++ t.getMessage).getOrElse("")
}

object LogMessage {
  def info(msg: String, region: sAst.Region, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Info, region, cause)
  def warning(msg: String, region: sAst.Region, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Warning, region, cause)
  def error(msg: String, region: sAst.Region, cause: Option[Throwable] = None) = LogMessage(msg, LogLevel.Error, region, cause)
}

case class Hooks(phook: Vector[sAst.Pragma => Eval.EvalState[List[sAst.Pragma]]] = Vector.empty,
                 ihook: Vector[sAst.InstanceExp => Eval.EvalState[List[sAst.InstanceExp]]] = Vector.empty,
                 chook: Vector[sAst.ConstructorDef => Eval.EvalState[List[sAst.ConstructorDef]]] = Vector.empty,
                 ahook: Vector[sAst.Assignment => Eval.EvalState[List[sAst.Assignment]]] = Vector.empty)
{
  def withPHooks(ps: (sAst.Pragma => Eval.EvalState[List[sAst.Pragma]])*) =
    copy(phook = phook ++ ps)

  def withIHooks(is: (sAst.InstanceExp => Eval.EvalState[List[sAst.InstanceExp]])*) =
    copy(ihook = ihook ++ is)

  def withCHooks(cs: (sAst.ConstructorDef => Eval.EvalState[List[sAst.ConstructorDef]])*) =
    copy(chook = chook ++ cs)

  def withAHooks(as: (sAst.Assignment => Eval.EvalState[List[sAst.Assignment]])*) =
    copy(ahook = ahook ++ as)
}

trait HooksOptics[H] {
  def withPHooks(h: H, ps: (sAst.Pragma => Eval.EvalState[List[sAst.Pragma]])*): H
  def withIHooks(h: H, is: (sAst.InstanceExp => Eval.EvalState[List[sAst.InstanceExp]])*): H
  def withCHooks(h: H, cs: (sAst.ConstructorDef => Eval.EvalState[List[sAst.ConstructorDef]])*): H
  def withAHooks(h: H, as: (sAst.Assignment => Eval.EvalState[List[sAst.Assignment]])*): H
}

case class EvalContext(prgms: Map[sAst.Identifier, List[sAst.Pragma]] = Map.empty,
                       cstrs: Map[sAst.Identifier, List[sAst.ConstructorDef]] = Map.empty,
                       vlxps: Map[sAst.Identifier, List[sAst.ValueExp]] = Map.empty,
                       insts: Map[sAst.Identifier, List[sAst.InstanceExp]] = Map.empty,
                       qnams: Map[sAst.LocalName, Set[sAst.QName]] = Map.empty,
                       hooks: Hooks = Hooks(),
                       logms: Seq[LogMessage] = Seq.empty)
{

  def withQNams(qs: sAst.QName*) =
    copy(qnams = qs.foldLeft(qnams) { case (m, q) => m + (q.localName -> (m.getOrElse(q.localName, Set.empty) + q))})

  def withConstructors(cs: sAst.ConstructorDef*) =
    copy(cstrs = cstrs ++ cs.map(c => c.id -> (c :: cstrs.getOrElse(c.id, Nil)))).withQNams(AllQNames.in(cs) :_*)

  def withAssignments(as: sAst.Assignment*) =
    copy(vlxps = vlxps ++ as.map(a => a.property -> (a.value :: vlxps.getOrElse(a.property, Nil)))).withQNams(AllQNames.in(as) :_*)

  def withPragmas(ps: sAst.Pragma*) =
    copy(prgms = prgms ++ ps.map(p => p.id -> (p :: prgms.getOrElse(p.id, Nil))))

  def withInstances(is: sAst.InstanceExp*) =
    copy(insts = insts ++ is.map(i => i.id -> (i :: insts.getOrElse(i.id, Nil)))).withQNams(AllQNames.in(is) :_*)

  def withLog(lm: LogMessage*) =
    copy(logms = logms ++ lm)

  def resolveLocalName(ln: sAst.LocalName): Set[sAst.QName] =
    qnams.getOrElse(ln, Set.empty)


  def resolveValue(id: sAst.Identifier): Option[sAst.ValueExp] =
    vlxps get id map (_.head) orElse { // todo: log if there are multiple elements in the list
      id match {
        case ln : sAst.LocalName =>
          (resolveLocalName(ln) map sAst.ValueExp.Identifier).headOption // todo: log clashes
        case _ => None
      }
    }

  def resolveCstr(id: sAst.Identifier): Option[sAst.ConstructorDef] =
    cstrs get id map (_.head) orElse { // todo: log if there are multiple elements in the list
      id match {
        case ln : sAst.LocalName =>
          (resolveLocalName(ln) flatMap resolveCstr).headOption // todo: log clashes
        case _ => None
      }
    }

  def resolveInst(id: sAst.Identifier): Option[sAst.InstanceExp] =
    insts get id map (_.head) orElse { // todo: log if there are multiple elements in the list
      id match {
        case ln : sAst.LocalName =>
          (resolveLocalName(ln) flatMap resolveInst).headOption // todo: log clashes
        case _ => None
      }
    }

  def withPHooks(ps: (sAst.Pragma => Eval.EvalState[List[sAst.Pragma]])*) =
    copy(hooks = hooks.withPHooks(ps :_*))

  def withIHooks(is: (sAst.InstanceExp => Eval.EvalState[List[sAst.InstanceExp]])*) =
    copy(hooks = hooks.withIHooks(is :_*))

  def withCHooks(cs: (sAst.ConstructorDef => Eval.EvalState[List[sAst.ConstructorDef]])*) =
    copy(hooks = hooks.withCHooks(cs :_*))

  def withAHooks(as: (sAst.Assignment => Eval.EvalState[List[sAst.Assignment]])*) =
    copy(hooks = hooks.withAHooks(as :_*))
}

sealed trait Eval[T] {
  self =>
  type Result
  def apply(t: T): Eval.EvalState[Result]
  def log(msg: String): Eval.Aux[T, self.Result] = new Eval[T] {
    override type Result = self.Result
    override def apply(t: T) = for {
      tt <- self.apply(t)
    } yield {
      println(s"$msg:\n\tin:  $t\n\tout: $tt")
      tt
    }
  }
}



import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, lens}
import shortbol.shapeless._
import scalaz.Scalaz._
import scalaz._


object Eval {
  type Aux[T, R] = Eval[T] { type Result = R }


  private object TypeclassFactory extends TypeClassCompanion2[Aux] {
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

      override val emptyCoproduct = Eval.identityEval[CNil]

      override val emptyProduct = Eval.identityEval[HNil]

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
  }

  import TypeclassFactory._

  type EvalState[R] = State[EvalContext, R]

  def log(logMessage: LogMessage) = modify((_: EvalContext).withLog(logMessage))
  def withPHooks(pHook: sAst.Pragma => Eval.EvalState[List[sAst.Pragma]]) = modify((_: EvalContext).withPHooks(pHook))
  def withIHooks(iHook: sAst.InstanceExp => Eval.EvalState[List[sAst.InstanceExp]]) = modify((_: EvalContext).withIHooks(iHook))
  def withCHooks(cHook: sAst.ConstructorDef => Eval.EvalState[List[sAst.ConstructorDef]]) = modify((_: EvalContext).withCHooks(cHook))
  def withAHooks(aHook: sAst.Assignment => Eval.EvalState[List[sAst.Assignment]]) = modify((_ : EvalContext).withAHooks(aHook))

  def constantEval[T, U](u: U) = new Eval[T] {
    override type Result = U
    override def apply(t: T) = u.point[EvalState]
  }

  def identityEval[T] = new Eval[T] {
    override type Result = T
    override def apply(t: T) = t.point[EvalState]
  }

  implicit class EvalOps[T](val _t: T) extends AnyVal {
    def eval[U](implicit e: Aux[T, U]): EvalState[U] = e(_t)
    def evalLog[U](msg: String)(implicit e: Aux[T, U]): EvalState[U] = e.log(msg).apply(_t)
  }

  implicit def seq[T, U](implicit pa: Aux[T, U]): Aux[Seq[T], Seq[U]] =
    typeClass.project(implicitly[Aux[List[T], List[U]]], (_: Seq[T]).to[List], implicitly[List[U]<:<Seq[U]])


  // Smelly! Find a way to compute this
  implicit lazy val topLevel: Aux[sAst.TopLevel, List[sAst.TopLevel.InstanceExp]] = {
    type U = List[sAst.TopLevel.InstanceExp]:+:
      List[sAst.TopLevel.InstanceExp]:+:
      List[sAst.TopLevel.InstanceExp]:+:
      List[sAst.TopLevel.InstanceExp]:+:
      List[sAst.TopLevel.InstanceExp]:+:
      List[sAst.TopLevel.InstanceExp]:+:CNil
    val g = Generic[sAst.TopLevel]
    val e = TypeclassFactory[g.Repr, U]
    typeClass.project[sAst.TopLevel, g.Repr, List[sAst.TopLevel.InstanceExp], U](e, g.to, _.unify)
  }

  implicit val propertyValue: Aux[sAst.PropertyValue, sAst.PropertyValue] = new Eval[sAst.PropertyValue] {
    override type Result = sAst.PropertyValue

    override def apply(t: sAst.PropertyValue) = t match {
      case l@sAst.PropertyValue.Literal(_) =>
        (l: sAst.PropertyValue).point[EvalState]
      case n@sAst.PropertyValue.Nested(_) =>
        (n: sAst.PropertyValue).point[EvalState]
      case sAst.PropertyValue.Reference(r) =>
        for {
          v <- (sAst.ValueExp.Identifier(r) : sAst.ValueExp).eval
        } yield v match {
          case sAst.ValueExp.Identifier(i) =>
            sAst.PropertyValue.Reference(i)
          case sAst.ValueExp.Literal(l) =>
            sAst.PropertyValue.Literal(l)
        }
    }
  } log "propertyValue"

  implicit val propertyExp: Aux[sAst.PropertyExp, sAst.PropertyExp] = new Eval[sAst.PropertyExp] {
    override type Result = sAst.PropertyExp

    override def apply(t: sAst.PropertyExp) = for {
      p <- t.property.eval
      v <- t.value.eval
    } yield {
      val pe = sAst.PropertyExp(p, v)
      pe.region = t.region
      pe
    }
  }

  implicit val bodyStmt = TypeclassFactory[sAst.BodyStmt, sAst.BodyStmt]

  implicit val literal: Aux[sAst.Literal, sAst.Literal] = identityEval

  // fixme: see if this is redundant
  implicit val sbFile: Aux[sAst.SBFile, lAst.SBFile] = new Eval[sAst.SBFile] {
    override type Result = lAst.SBFile

    override def apply(t: sAst.SBFile) = for {
      ts <- t.tops.eval
    } yield {
      val f = lAst.SBFile(ts.flatten)
      f.region = t.region
      f
    }
  }

  implicit val blankLine: Aux[sAst.BlankLine, sAst.BlankLine] = identityEval

  implicit val topLevel_blankLine: Aux[sAst.TopLevel.BlankLine, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.BlankLine] {
    override type Result = List[sAst.TopLevel.InstanceExp]

    override def apply(t: sAst.TopLevel.BlankLine) = for {
      te <- t.blankLine.eval
    } yield Nil
  }

  implicit val comment: Aux[sAst.Comment, sAst.Comment] = identityEval

  implicit val topLevel_comment: Aux[sAst.TopLevel.Comment, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.Comment] {
      override type Result = List[sAst.TopLevel.InstanceExp]

      override def apply(t: sAst.TopLevel.Comment) = for {
        te <- t.comment.eval
      } yield Nil
    }

  implicit val topLevel_pragma: Aux[sAst.TopLevel.Pragma, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.Pragma] {
    override type Result = List[sAst.TopLevel.InstanceExp]

    override def apply(t: sAst.TopLevel.Pragma) = for {
      p <- t.pragma.eval
      _ <- modify((_: EvalContext).withPragmas(p :_*))
    } yield Nil
  }

  implicit val pragama: Aux[sAst.Pragma, List[sAst.Pragma]] = new Eval[sAst.Pragma] {
    override type Result = List[sAst.Pragma]

    override def apply(t: sAst.Pragma) = for {
      phook <- gets((_: EvalContext).hooks.phook)
      hook = phook.foldl((p: sAst.Pragma) => (List(p)).point[EvalState])(
        h1 => h2 => (p0: sAst.Pragma) => for {
          p1 <- h1(p0)
          p2 <- (p1 map h2).sequence
        } yield p2.flatten)
      cd <- hook(t)
    } yield cd
  }

  implicit val assignment: Aux[sAst.Assignment, sAst.Assignment] = new Eval[sAst.Assignment] {
    override type Result = sAst.Assignment

    override def apply(t: sAst.Assignment) = for {
      p <- t.property.eval
      v <- t.value.eval
    } yield {
      val a = sAst.Assignment(p, v)
      a.region = t.region
      a
    }
  }

  implicit val topLevel_assignment: Aux[sAst.TopLevel.Assignment, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.Assignment] {
    override type Result = List[sAst.TopLevel.InstanceExp]

    override def apply(t: sAst.TopLevel.Assignment) = for {
      ahook <- gets((_: EvalContext).hooks.ahook)
      hook = ahook.foldl((a: sAst.Assignment) => List(a).point[EvalState])(
        h1 => h2 => (a0 : sAst.Assignment) => for {
          a1 <- h1(a0)
          a2 <- (a1 map h2).sequence
        } yield a2.flatten)
      a <- hook(t.assignment)
      _ <- modify((_: EvalContext).withAssignments(a :_*))
    } yield Nil
  }

  implicit val topLevel_constructorDef: Aux[sAst.TopLevel.ConstructorDef, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.ConstructorDef] {
    override type Result = List[sAst.TopLevel.InstanceExp]

    override def apply(t: sAst.TopLevel.ConstructorDef) = for {
      cd <- t.constructorDef.eval
      _ <- modify((_: EvalContext).withConstructors(cd :_*))
    } yield Nil
  }

  implicit val constructorDef: Aux[sAst.ConstructorDef, List[sAst.ConstructorDef]] = new Eval[sAst.ConstructorDef] {
    override type Result = List[sAst.ConstructorDef]

    override def apply(t: sAst.ConstructorDef) = for {
      chook <- gets((_: EvalContext).hooks.chook)
      hook = chook.foldl((c: sAst.ConstructorDef) => List(c).point[EvalState])(
        h1 => h2 => (c0: sAst.ConstructorDef) => for {
          c1 <- h1(c0)
          c2 <- (c1 map h2).sequence
        } yield c2.flatten)
      cd <- hook(t)
    } yield cd
  }

  implicit val constructorApp: Aux[sAst.ConstructorApp, sAst.ConstructorApp] = new Eval[sAst.ConstructorApp] {
    override type Result = sAst.ConstructorApp

    override def apply(ca: sAst.ConstructorApp) = for {
      tcTcBody <- ca.cstr.eval
      (tc, tcBody) = tcTcBody
      body <- ca.body.eval
    } yield {
      val c = sAst.ConstructorApp(tc, tcBody ++ body)
      c.region = ca.region
      c
    }
  } log "constructorApp"


  implicit val instanceExp: Aux[sAst.InstanceExp, List[sAst.InstanceExp]] = new Eval[sAst.InstanceExp] {
    override type Result = List[sAst.InstanceExp]

    override def apply(i: sAst.InstanceExp) = for {
      ce <- i.cstrApp.eval
      ihook <- gets((_: EvalContext).hooks.ihook)
      hook = ihook.foldl((i: sAst.InstanceExp) => List(i).point[EvalState])(
        h1 => h2 => (i0: sAst.InstanceExp) => for
        {
          i1 <- h1(i0)
          i2 <- (i1 map h2).sequence
        } yield i2.flatten)
      ie = sAst.InstanceExp(i.id, ce)
      _ = ie.region = i.region
      is <- hook(ie)
    } yield is
  }

  implicit val topLevel_instanceExp: Aux[sAst.TopLevel.InstanceExp, List[sAst.TopLevel.InstanceExp]] = new Eval[sAst.TopLevel.InstanceExp] {
    override type Result = List[sAst.TopLevel.InstanceExp]

    override def apply(t: sAst.TopLevel.InstanceExp) = for {
      is <- t.instanceExp.eval
      _ <- modify((_: EvalContext).withInstances(is :_*))
    } yield is map sAst.TopLevel.InstanceExp
  }

  implicit val constructorDefApp: Aux[(Seq[sAst.ValueExp], sAst.ConstructorDef), (sAst.TpeConstructor, Seq[sAst.BodyStmt])] = new Eval[(Seq[sAst.ValueExp], sAst.ConstructorDef)] {
    override type Result = (sAst.TpeConstructor, Seq[sAst.BodyStmt])

    override def apply(vscd: (Seq[sAst.ValueExp], sAst.ConstructorDef)) = for {
      cd <- withStack(vscd._2.args, vscd._1)(vscd._2.cstrApp.evalLog("defCstrApp"))
    } yield (cd.cstr, cd.body)

    def withStack[T](names: Seq[sAst.Identifier], values: Seq[sAst.ValueExp])(sf: EvalState[T]) = for {
      ec <- get[EvalContext]
      _ <- modify ((_: EvalContext).withAssignments(names zip values map (sAst.Assignment.apply _).tupled :_*))
      v <- sf
      _ <- put(ec) // fixme: should we only be only overwriting the bindings?
    } yield v
  } log "constructorDefApp"

  implicit val tpeConstructor1: Aux[sAst.TpeConstructor1, (sAst.TpeConstructor, Seq[sAst.BodyStmt])] = new Eval[sAst.TpeConstructor1] {
    override type Result = (sAst.TpeConstructor, Seq[sAst.BodyStmt])

    override def apply(t: sAst.TpeConstructor1) = for {
      ot <- resolveWithAssignment(t.id)
      args <- t.args.evalLog("tArgs")
      ts <- ot match {
        case Some(cd) =>
          (args, cd).evalLog("argsCd")
        case None =>
          for {
            id <- t.id.eval
          } yield {
            val t1 = sAst.TpeConstructor1(id, args)
            t1.region = t.region
            (t1, Seq.empty)
          }
      }
    } yield ts

    def resolveWithAssignment(id: sAst.Identifier): EvalState[Option[sAst.ConstructorDef]] = for {
      c <- cstr(id)
      cc <- c match {
        case Some(_) =>
          c.point[EvalState]
        case None =>
          for {
            b <- resolveBinding(id)
            bb <- b match {
              case Some(sAst.ValueExp.Identifier(nid)) =>
                resolveWithAssignment(nid)
              case _ =>
                (None : Option[sAst.ConstructorDef]).point[EvalState]
            }
          } yield bb
      }
    } yield cc
  } log "tpeConstructor1"

  implicit val tpeConstructorStar: Aux[sAst.TpeConstructorStar, (sAst.TpeConstructor, Seq[sAst.BodyStmt])] = new Eval[sAst.TpeConstructorStar] {
    override type Result = (sAst.TpeConstructor, Seq[sAst.BodyStmt])

    override def apply(t: sAst.TpeConstructorStar) = ((t: sAst.TpeConstructor) -> Seq.empty[sAst.BodyStmt]).point[EvalState]
  }

  implicit val tpeConstructor: Aux[sAst.TpeConstructor, (sAst.TpeConstructor, Seq[sAst.BodyStmt])] =
  {
    type U = (sAst.TpeConstructor, Seq[sAst.BodyStmt]):+:(sAst.TpeConstructor, Seq[sAst.BodyStmt]):+:CNil
    val g = Generic[sAst.TpeConstructor]
    val e = TypeclassFactory[g.Repr, U]
    typeClass.project[sAst.TpeConstructor, g.Repr, (sAst.TpeConstructor, Seq[sAst.BodyStmt]), U](e, g.to, _.unify)
  }

  implicit val identifier: Aux[sAst.Identifier, sAst.Identifier] = new Eval[sAst.Identifier] {
    override type Result = sAst.Identifier

    def apply(id: sAst.Identifier) = resolveWithAssignment(id)

    def resolveWithAssignment(id: sAst.Identifier): State[EvalContext, sAst.Identifier] = for {
      b <- resolveBinding(id)
      rb <- b match {
        case Some(sAst.ValueExp.Identifier(rid)) =>
          resolveWithAssignment(rid)
        case _ =>
          id.point[EvalState]
      }
    } yield rb
  }

  implicit val valueExp: Aux[sAst.ValueExp, sAst.ValueExp] = new Eval[sAst.ValueExp] {
    override type Result = sAst.ValueExp

    override def apply(t: sAst.ValueExp) = t match {
      case sAst.ValueExp.Literal(l) =>
        for {
          ll <- l.eval
        } yield sAst.ValueExp.Literal(ll)
      case sAst.ValueExp.Identifier(i) =>
        resolveWithAssignment(i)
    }

    def resolveWithAssignment(id: sAst.Identifier): State[EvalContext, sAst.ValueExp] = for {
      b <- resolveBinding(id)
      rb <- b match {
        case Some(sAst.ValueExp.Identifier(rid)) =>
          resolveWithAssignment(rid)
        case Some(l@sAst.ValueExp.Literal(_)) =>
          l.point[EvalState]
        case None =>
          sAst.ValueExp.Identifier(id).point[EvalState]
      }
    } yield rb
  }

  def as[T, U](implicit e: Aux[U, U], to: T <:< U): Aux[T, U] =
    typeClass.project[T, U, U, U](e, to, identity)

  implicit val localName: Aux[sAst.LocalName, sAst.Identifier] = as[sAst.LocalName, sAst.Identifier]
  implicit val qname: Aux[sAst.QName, sAst.Identifier] = as[sAst.QName, sAst.Identifier]
  implicit val url: Aux[sAst.Url, sAst.Identifier] = as[sAst.Url, sAst.Identifier]

  def cstr(id: sAst.Identifier): State[EvalContext, Option[sAst.ConstructorDef]] =
    gets ((_: EvalContext).resolveCstr(id))

  def resolveBinding(id: sAst.Identifier): State[EvalContext, Option[sAst.ValueExp]] =
    gets ((_: EvalContext).resolveValue(id))

}
