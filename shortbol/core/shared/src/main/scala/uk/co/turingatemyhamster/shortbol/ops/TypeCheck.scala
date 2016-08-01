package uk.co.turingatemyhamster.shortbol
package ops

import scala.reflect.runtime.universe.TypeTag
import ast._
import sugar._

import scalaz.{-\/, Applicative, IList, NonEmptyList, Scalaz, Validation, ValidationNel, \/, \/-}
import Scalaz._
import monocle._
import monocle.function.FilterIndex
import monocle.macros._


sealed trait ConstraintViolation[A] {
  def at: A

  def in[X, K](in: X, key: K, setter: Option[Setter[X, A]])
              (implicit aTpe: TypeTag[X], kTpe: TypeTag[K], bTpe: TypeTag[A]): ConstraintViolation[X] =
    NestedViolation[X, K, A](in, key, this)(setter)

  def prettyPrint: String

  def recoverWith[C <: ConstraintViolation[X], X](r: Recovery[C, X]): Option[A]
}

case class Recovery[C <: ConstraintViolation[X], X](f: C => Option[X])(implicit val cTT: TypeTag[C], val xTT: TypeTag[X])

object ConstraintViolation {
  def failure[A](rule: Constraint[A], at: A)(implicit aTT: TypeTag[A]): ConstraintViolation[A] =
    ConstraintFailure(rule, at)
}

case class ConstraintFailure[A](rule: Constraint[A], at: A)
                               (implicit cfaTT: TypeTag[ConstraintFailure[A]], aTT: TypeTag[A]) extends ConstraintViolation[A] {
  override def prettyPrint: String = s"failed(${rule.prettyPrint} at $at"

  override def recoverWith[C <: ConstraintViolation[X], X](r: Recovery[C, X]) = {
    println("Failure:")
    println(cfaTT)
    println(r.cTT)
    println(aTT)
    println(r.xTT)
    if(cfaTT.tpe <:< r.cTT.tpe && aTT.tpe <:< r.xTT.tpe) {
      println("Applying matching recovery:")
      val x = r.f(this.asInstanceOf[C]).map(_.asInstanceOf[A])
      println(s"Recovered to: $x")
      x
    } else {
      None
    }
  }
}

case class NestedViolation[A, K, B](at: A, key: K, because: ConstraintViolation[B])(setter: Option[Setter[A, B]])
                                   (implicit val nvTT: TypeTag[NestedViolation[A, K, B]], aTT: TypeTag[A]) extends ConstraintViolation[A] {
  def prettyIn: String = {
    val s = at.toString
    if(s.length > 40) s.substring(0, 36) + " ..." else s
  }
  override def prettyPrint: String = s"violation($key of $prettyIn because ${because.prettyPrint})"

  override def recoverWith[C <: ConstraintViolation[X], X](r: Recovery[C, X]) = {
    println("Nested failure:")
    println(nvTT)
    println(r.cTT)
    println(aTT)
    println(r.xTT)
    if(nvTT.tpe <:< r.cTT.tpe && aTT.tpe <:< r.xTT.tpe) {
      println("Applying matching recovery:")
      val x = r.f(this.asInstanceOf[C]).map(_.asInstanceOf[A])
      println(s"Recovered to: $x")
      x
    } else {
      if(setter.isEmpty) println(s"Stopping here as we have no setter to traverse $key into ${because.prettyPrint}.")
      for {
        s <- setter
        b <- because.recoverWith(r)
      } yield s.set(b)(at)
    }
  }
}

import Constraint.ValidatedConstraints

trait Constraint[A] {
  self =>

  def apply(a: A): ValidatedConstraints[A]

  final def onlyIf(c: Constraint[A]) = If(c, this, Constraint.success)
  final def unless(c: Constraint[A]) = If(c, Constraint.success, this)

  final def requires(c: Constraint[A])(implicit aTT: TypeTag[A]) = If(c, this, Constraint.fail[A])
  final def rejecting(c: Constraint[A])(implicit aTT: TypeTag[A]) = If(c, Constraint.fail[A], this)

  final def @: [O, X](o: O)(implicit ocb: OpticConstraintBuilder[O, X, A]): Constraint[X] = this match {
    case AlwaysSucceed() => Constraint.success[X]
    case c => ocb(o, c)
  }

  def prettyPrint: String

  def log(msg: String): Constraint[A] = new Constraint[A] {
    override def apply(a: A) = {
      println(s"--> $msg: $a")
      val v = self(a)
      println(s"<-- $msg: $a ~> $v")
      v
    }

    override def prettyPrint = self.prettyPrint

    override def log(msg: String) = self
  }
}

object Constraint {

  type ValidatedConstraints[A] = ValidationNel[ConstraintViolation[A], A]

  def success[A] = AlwaysSucceed[A]()
  def fail[A](implicit aTT: TypeTag[A]) = AlwaysFail[A](none)


  def applyAll[A](cs: List[Constraint[A]])(implicit aTT: TypeTag[A]): Constraint[A] =
    cs flatMap {
      case AlwaysSucceed() => Nil
      case ApplyAll(xx) => xx
      case x => x :: Nil
    } match {
      case Nil => success[A]
      case h::Nil => h
      case css => ApplyAll(css)
    }

  def forEvery[A](c: Constraint[A])(implicit aTT: TypeTag[A]): Constraint[List[A]] = c match {
    case AlwaysSucceed() => success[List[A]]
    case _ => ForEvery(c)
  }

  def forAny[A](c: Constraint[A])(implicit aTT: TypeTag[A]): Constraint[List[A]] = c match {
    case AlwaysSucceed() => success[List[A]]
    case _ => ForAny(c)
  }
}

trait OpticConstraintBuilder[O, A, B] {
  def apply(o: O, c: Constraint[B]): Constraint[A]
}

object OpticConstraintBuilder {
  implicit def prismBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Prism[A, B]), A, B] {
    override def apply(o: (K, Prism[A, B]), c: Constraint[B]) = InPrism[A, B, K](o._1, c)(o._2)
  }

  implicit def lensBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Lens[A, B]), A, B] {
    override def apply(o: (K, Lens[A, B]), c: Constraint[B]) = InLens[A, B, K](o._1, c)(o._2)
  }

  implicit def optionalBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Optional[A, B]), A, B] {
    override def apply(o: (K, Optional[A, B]), c: Constraint[B]) = InOptional[A, B, K](o._1, c)(o._2)
  }

  implicit def getterBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Getter[A, B]), A, B] {
    override def apply(o: (K, Getter[A, B]), c: Constraint[B]) = InGetter[A, B, K](o._1, c)(o._2)
  }

  implicit def functionBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, A => B), A, B] {
    override def apply(o: (K, A => B), c: Constraint[B]) = InGetter[A, B, K](o._1, c)(Getter(o._2))
  }
//
//  implicit def traversalBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Traversal[A, B]), A, B] {
//    override def apply(o: (K, Traversal[A, B]),
//                       c: Constraint[B]) = ???
//  }

  implicit def traversalAllBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Traversal[A, B]), A, List[B]] {

    override def apply(o: (K, Traversal[A, B]),
                       c: Constraint[List[B]]) = (o._1, o._2.getAll _) @: c
  }
//
//  implicit def foldBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Fold[A, B]), A, B] {
//
//    override def apply(o: (K, Fold[A, B]),
//                       c: Constraint[B]) = (o._1, o._2.getAll _) @: c
//  }
}

case class AlwaysSucceed[A]() extends Constraint[A] {
  override def apply(a: A) = a.successNel

  override def prettyPrint: String = "success"
}


case class AlwaysFail[A](msg: Option[String])(implicit aTT: TypeTag[A]) extends Constraint[A] {
  override def apply(a: A) = ConstraintViolation.failure(this, a).failureNel[A]

  override def prettyPrint: String = "failure"
}

case class If[A](condition: Constraint[A], ifTrue: Constraint[A], ifFalse: Constraint[A]) extends Constraint[A] {
  override def apply(a: A) = condition(a).fold(
    _ => ifFalse(a),
    _ => ifTrue(a)
  )

  override def prettyPrint: String = s"if(${condition.prettyPrint} then ${ifTrue.prettyPrint} else ${ifFalse.prettyPrint})"
}

case class ApplyAll[A](cs: List[Constraint[A]]) extends Constraint[A] {
  override def apply(a: A) = {
    val (aa, errs) = cs.foldLeft((a, List.empty[ConstraintViolation[A]]))(applyConstraint)

    errs match {
      case h::t => NonEmptyList(h, t :_*).failure[A]
      case Nil => aa.successNel[ConstraintViolation[A]]
    }
  }

  def applyConstraint(ae: (A, List[ConstraintViolation[A]]), c: Constraint[A]): (A, List[ConstraintViolation[A]]) =
    c(ae._1).fold(
      e => (ae._1, ae._2 ++ e.toList),
      s => (s, ae._2)
    )

  override def prettyPrint: String = s"applyAll(${cs map (_.prettyPrint) mkString " "})"
}

case class ForEvery[A](c: Constraint[A])(implicit aTT: TypeTag[A]) extends Constraint[List[A]] {
  override def apply(as: List[A]) = as match {
    case Nil =>
      as.successNel
    case _ =>
      val idx = monocle.std.list.listIndex[A]
      (NonEmptyList.nel(as.head, IList.fromList(as.tail)).zipWithIndex map { case(a, i) =>
        c apply a leftMap (e => e map (_.in(as, i, idx.index(i).asSetter.some))) }
        ).sequenceU map (_.list.toList)
  }

  override def prettyPrint: String = s"forEvery(${c.prettyPrint})"
}

case class ForAny[A](c: Constraint[A])(implicit aTT: TypeTag[A]) extends Constraint[List[A]] {
  override def apply(as: List[A]) = as match {
    case Nil =>
      as.successNel
    case _ => // fixme: we're loosing logging here
      val idx = monocle.std.list.listIndex[A]
      val apps = as map c.apply
      apps.filter(_.isSuccess) headOption match {
        case Some(s) =>
          as.successNel
        case None =>
          ConstraintViolation.failure(this, as).failureNel
      }
  }

  override def prettyPrint = s"forAny(${c.prettyPrint}"
}

case class EqualTo[A](eA: A)(implicit aTT: TypeTag[A]) extends Constraint[A] {
  override def apply(a: A) =
    if(eA == a) a.successNel else ConstraintViolation.failure(this, a).failureNel

  override def prettyPrint: String = s"($eA == _)"
}

case class MemberOf[A](a: A)(implicit aTT: TypeTag[A]) extends Constraint[Set[A]] {
  override def apply(as: Set[A]) =
    if(as contains a) as.successNel else ConstraintViolation.failure(this, as).failureNel

  override def prettyPrint: String = s"($a in _)"
}

case class NotLessThan[A](min: Int) extends Constraint[Int] {
  override def apply(a: Int) =
    if(a < min) ConstraintViolation.failure(this, a).failureNel else a.successNel

  override def prettyPrint: String = s"(_ >= $min)"
}

case class NotGreaterThan[A](max: Int) extends Constraint[Int] {
  override def apply(a: Int) =
    if(a > max) ConstraintViolation.failure(this, a).failureNel else a.successNel

  override def prettyPrint: String = s"(_ <= $max)"
}

case class InPrism[A, B, K](key: K, bC: Constraint[B])
                           (prism: Prism[A, B])
                           (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    prism.getOption(a) match {
      case Some(b) =>
        bC(b).leftMap(_.map(_.in(a, key, prism.asSetter.some))).map(b => prism.set(b)(a))
      case None =>
        a.successNel
    }

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class InLens[A, B, K](key: K, bC: Constraint[B])
                          (lens: Lens[A, B])
                          (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    bC(lens.get(a)).leftMap(_.map(_.in(a, key, lens.asSetter.some))).map(b => lens.set(b)(a))

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class InOptional[A, B, K](key: K, bC: Constraint[B])
                              (optional: Optional[A, B])
                              (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    optional.getOption(a) match {
      case Some(b) =>
        bC(b).leftMap(_.map(_.in(a, key, optional.asSetter.some))).map(b => optional.set(b)(a))
      case None =>
        a.successNel
    }

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class InGetter[A, B, K](key: K, bC: Constraint[B])
                            (getter: Getter[A, B])
                            (implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    bC(getter.get(a)).leftMap(_.map(_.in(a, key, none))).map(_ => a)

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

trait ConstraintSystem {
  def fromContext(ctxt: EvalContext): Constraint[TopLevel.InstanceExp]
}

object ConstraintSystem {
  def apply(cs: ConstraintSystem*) = new {
    def apply(recovery: Recovery[_, _]*) = new {
      def apply(ctxt: EvalContext) = new Constraint[SBEvaluatedFile] {
        val allConstraints = Constraint.applyAll(cs.to[List] map (_.fromContext(ctxt)))

        val recoveringConstraint = new Constraint[TopLevel.InstanceExp] {
          override def apply(a: TopLevel.InstanceExp): ValidatedConstraints[TopLevel.InstanceExp] =
            applyWithRecovery(a).validationNel

          def applyWithRecovery(a: TopLevel.InstanceExp): ConstraintViolation[TopLevel.InstanceExp] \/ TopLevel.InstanceExp =
            allConstraints(a).fold(
                        err => {
                          println(s"*** applyWithRecovery *** at $a for error ${err.head.prettyPrint}")
                          val ar = attemptRecovery(err.head)
                          println(s"*** recovered *** to ${ar.fold(_.prettyPrint, _.toString)}")
                          val aar = ar flatMap applyWithRecovery
                          println(s"*** finally *** $aar")
                          aar
                        },
                        \/-(_)
                      )

          def attemptRecovery(cv: ConstraintViolation[TopLevel.InstanceExp]): ConstraintViolation[TopLevel.InstanceExp] \/ TopLevel.InstanceExp =
            recovery.flatMap(
              cv recoverWith _.asInstanceOf[Recovery[ConstraintViolation[Any], Any]] // type hack -- should do better
            ).headOption \/> cv

          override def prettyPrint = allConstraints.prettyPrint
        } log "recoveringConstraint"

        val sbEFConstraint = (
          ('tops, optics.sbEvaluatedFile.tops) @:
          (ForEvery(
            recoveringConstraint
          ) log "tops")
          ) log "sbEFConstraint"

        def apply(sev: SBEvaluatedFile) = sbEFConstraint(sev)

        override def prettyPrint = sbEFConstraint.prettyPrint
      }

      def apply(cf : (EvalContext, SBEvaluatedFile))
      : ValidatedConstraints[SBEvaluatedFile] = apply(cf._1)(cf._2)
    }
  }
}

object optics {

  def seqListIso[A] = Iso[Seq[A], List[A]](_.to[List])(_.to[Seq])

  object instanceExp {
    val id = GenLens[InstanceExp](_.id)
    val cstrApp = GenLens[InstanceExp](_.cstrApp)
  }


  object constructorApp {
    val cstr = GenLens[ConstructorApp](_.cstr)
    val body = GenLens[ConstructorApp](_.body) composeIso seqListIso[BodyStmt]

//    def propertyFilter(property: Identifier) = new PTraversal[ConstructorApp, ConstructorApp, PropValue, PropValue] {
//      override def modifyF[F[_] : Applicative](f: (PropValue) => F[PropValue])
//                                              (s: ConstructorApp) =
//      {
//        body.modifyF[F](
//          _.traverse {
//            case a@BodyStmt.Assignment(Assignment(p, v)) =>
//              if(p == property)
//                f(Left(v) : PropValue) map {
//                  case Left(vv) => BodyStmt.Assignment(Assignment(p, vv))
//                }
//              else
//                Applicative[F].point(a)
//            case ie@BodyStmt.InstanceExp(InstanceExp(i, c)) =>
//              if(i == property)
//                f(Right(c) : PropValue) map {
//                  case Right(cc) => BodyStmt.InstanceExp(InstanceExp(i, cc))
//                }
//              else
//                Applicative[F].point(ie)
//          }
//        )(s)
//      }
//    }
  }

  object bodyStmt {
    type PropValue = Either[ValueExp, ConstructorApp]

    val assignment = GenPrism[BodyStmt, BodyStmt.Assignment] composeLens
      GenLens[BodyStmt.Assignment](_.assignment)
    val blankLine = GenPrism[BodyStmt, BodyStmt.BlankLine] composeLens
      GenLens[BodyStmt.BlankLine](_.blankLine)
    val comment = GenPrism[BodyStmt, BodyStmt.Comment] composeLens
      GenLens[BodyStmt.Comment](_.comment)
    val instanceExp = GenPrism[BodyStmt, BodyStmt.InstanceExp] composeLens
      GenLens[BodyStmt.InstanceExp](_.instanceExp)
    val constructorApp = GenPrism[BodyStmt, BodyStmt.ConstructorApp] composeLens
      GenLens[BodyStmt.ConstructorApp](_.constructorApp)

    val property: Optional[BodyStmt, (Identifier, PropValue)] = new POptional[BodyStmt, BodyStmt, (Identifier, PropValue), (Identifier, PropValue)] {
      override def getOrModify(s: BodyStmt) = {
        s match {
          case BodyStmt.Assignment(Assignment(p, v)) =>
            \/-(p -> (Left(v) : PropValue))
          case BodyStmt.InstanceExp(InstanceExp(i, c)) =>
            \/-(i -> (Right(c) : PropValue))
          case bs =>
            -\/(bs)
        }
      }

      override def set(b: (Identifier, PropValue)) = s => b match {
        case (p, Left(v)) =>
          BodyStmt.Assignment(Assignment(p, v))
        case (i, Right(c)) =>
          BodyStmt.InstanceExp(InstanceExp(i, c))
      }

      override def getOption(s: BodyStmt) = getOrModify(s) fold (
       _ => None,
        Some(_)
      )
      def modifyF[F[_]: Applicative](f: ((Identifier, PropValue)) => F[(Identifier, PropValue)])(s: BodyStmt): F[BodyStmt] =
        getOption(s).fold(
          Applicative[F].point(s))(
          a => Applicative[F].map(f(a))(set(_)(s))
        )

      def modify(f: ((Identifier, PropValue)) => (Identifier, PropValue)): BodyStmt => BodyStmt =
        s => getOption(s).fold(s)(a => set(f(a))(s))
    }
  }

  object assignment {
    val property = GenLens[Assignment](_.property)
    val value = GenLens[Assignment](_.value)
  }

  object valueExp {
    val identifier = GenPrism[ValueExp, ValueExp.Identifier] composeLens
      GenLens[ValueExp.Identifier](_.identifier)
    val literal = GenPrism[ValueExp, ValueExp.Literal] composeLens
      GenLens[ValueExp.Literal](_.literal)
  }

  object topLevel {
    object instanceExp {
      val instanceExp = GenLens[TopLevel.InstanceExp](_.instanceExp)
    }
  }

  object sbFile {
    val tops = GenLens[SBFile](_.tops) composeIso seqListIso[TopLevel]
  }

  object sbEvaluatedFile {
    val tops = GenLens[SBEvaluatedFile](_.tops) composeIso seqListIso[TopLevel.InstanceExp]
  }

  object list {

    implicit def listSublist[A] = Optional({(lse: (List[A], Int, Int)) =>
      val (list, start, end) = lse
      val sublist = list.slice(start, end)
      if(sublist.length == end - start)
        Some(sublist)
      else
        None
    })({sublist => lse =>
      val (list, start, end) = lse
      val (pfx, tail) = list splitAt end
      val (head, dead) = pfx splitAt start
      (head ::: sublist ::: tail, start, (end - pfx.length + sublist.length))
    })
  }

  def unsafeSelect[A](predicate: A => Boolean): Prism[A, A] =
    Prism[A, A](a => if (predicate(a)) Some(a) else None)(a => a)
}



