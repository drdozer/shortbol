package uk.co.turingatemyhamster.shortbol
package ops

import scala.reflect.runtime.universe.TypeTag
import _root_.shapeless._

import longhandAst._


sealed trait Violation[Path <: HList, Value] {
  def path: Path
  def value: Value

  def prettyPrint: StringBuilder

  def @: [Step, Within](s: Step, w: Within) = NestedViolation(s, w, this)
}

case class NestedViolation[Step, Suffix <: HList, Within, At](step: Step, value: Within, because: Violation[Suffix, At])
  extends Violation[Step :: Suffix, Within]
{
  val path: Step :: Suffix = step :: because.path

  override def prettyPrint: StringBuilder = because.prettyPrint append " at " append step
}

case class ConstraintViolation[Value](value: Value, cause: Constraint[Value]) extends Violation[HNil, Value] {
  override def path: HNil = HNil

  override def prettyPrint: StringBuilder = new StringBuilder() append cause.prettyPrint append " in " append value
}

trait Constraint[At] {
  self =>

  type Path <: HList

  def apply(at: At): List[Violation[Path, At]]

  def prettyPrint: StringBuilder

  final def @:[T, P <: HList, V](t: T)(implicit b: ConstraintBuilder[T, P, Path, V, At]): Constraint.Aux[P, V] = b(t, this)

  def && (other: Constraint.Aux[Path, At]) = new Constraint[At] {
    override type Path = self.Path

    override def apply(at: At): List[Violation[Path, At]] = self(at) ++ other(at)

    override def prettyPrint: StringBuilder = self.prettyPrint append other.prettyPrint
  }
}

object Constraint {
  type Aux[P <: HList, At] = Constraint[At] { type Path = P }
}

trait ConstraintBuilder[T, Path <: HList, Suffix <: HList, Within, At] {
  def apply(t: T, cat: Constraint.Aux[Suffix, At]): Constraint.Aux[Path, Within]
}

object ConstraintBuilder {

  implicit def fromSfxGetter[Step, Suffix <: HList, Within, At]: ConstraintBuilder[(Step, Within => At), Step :: Suffix, Suffix, Within, At] = new ConstraintBuilder[(Step, (Within) => At), ::[Step, Suffix], Suffix, Within, At] {
    override def apply(t: (Step, (Within) => At),
                       cat: Constraint.Aux[Suffix, At]): Constraint.Aux[Step :: Suffix, Within] = new Constraint[Within] {
      override type Path = Step :: Suffix

      override def apply(within: Within): scala.List[Violation[Step :: Suffix, Within]] = cat(t._2(within)) map (_.@:(t._1, within))

      override def prettyPrint: StringBuilder = cat.prettyPrint append " at " append t._1
    }
  }

  import sharedAst.Identifier
  import longhandAst.PropertyExp
  implicit def fromProperty[I <: Identifier, Suffix <: HList]: ConstraintBuilder[I, (I, List[Int]) :: Suffix, Suffix, List[PropertyExp], List[PropertyExp]] = new ConstraintBuilder[I, ::[(I, List[Int]), Suffix], Suffix, List[PropertyExp], List[PropertyExp]] {

    override def apply(propId: I,
                       cat: Constraint.Aux[Suffix, List[PropertyExp]]): Constraint.Aux[(I, List[Int]) :: Suffix, List[PropertyExp]] = new Constraint[List[PropertyExp]] {
      override type Path = (I, List[Int]) :: Suffix

      override def apply(at: List[PropertyExp]): List[Violation[Path, List[PropertyExp]]] = {
        val (is, pes) = (for {
          (pe@PropertyExp(p, _), i) <- at.zipWithIndex if p == propId
        } yield (i, pe)).unzip

        cat(pes) map (_.@:(propId -> is, at))
      }

      override def prettyPrint: scala.StringBuilder = cat.prettyPrint append " at " append propId
    }
  }
}

//
//import Constraint.ValidatedConstraints
//
//  self =>
//
//  def apply(a: A): ValidatedConstraints[A]
//
//  final def onlyIf(c: => Constraint[A])(implicit aTT: TypeTag[A]) = If(c, this, Constraint.success)
//  final def unless(c: => Constraint[A])(implicit aTT: TypeTag[A]) = If(c, Constraint.success, this)
//
//  final def requires(c: => Constraint[A])(implicit aTT: TypeTag[A]) = If(c, this, Constraint.fail[A])
//  final def rejecting(c: => Constraint[A])(implicit aTT: TypeTag[A]) = If(c, Constraint.fail[A], this)
//
//  final def @: [O, X](o: O)(implicit ocb: OpticConstraintBuilder[O, X, A], xTT: TypeTag[X]): Constraint[X] = this match {
//    case AlwaysSucceed() => Constraint.success[X]
//    case c => ocb(o, c)
//  }
//
//  def prettyPrint: String
//
//  def not: Constraint[A]
//
//  def log(msg: String): Constraint[A] = new Constraint[A] {
//    override def apply(a: A) = {
//      println(s"--> $msg: $a")
//      val v = self(a)
//      println(s"<-- $msg: $a ~> $v")
//      v
//    }
//
//    override def not = ???
//
//    override def prettyPrint = self.prettyPrint
//
//    override def log(msg: String) = self
//  }
//}
//
//object Constraint {
//
//  type ValidatedConstraints[A] = ValidationNel[ConstraintViolation[A], A]
//
//  def success[A](implicit aTT: TypeTag[A]) = AlwaysSucceed[A]()
//  def fail[A](implicit aTT: TypeTag[A]) = AlwaysFail[A](none)
//  def fail[A](msg: String)(implicit aTT: TypeTag[A]) = AlwaysFail[A](msg.some)
//
//
//  def applyAll[A](cs: List[Constraint[A]])(implicit aTT: TypeTag[A]): Constraint[A] =
//    cs flatMap {
//      case AlwaysSucceed() => Nil
//      case ApplyAll(xx) => xx
//      case x => x :: Nil
//    } match {
//      case Nil => success[A]
//      case h::Nil => h
//      case css => ApplyAll(css)
//    }
//
//  def forEvery[A](c: Constraint[A])(implicit aTT: TypeTag[A]): Constraint[List[A]] = c match {
//    case AlwaysSucceed() => success[List[A]]
//    case _ => ForEvery(c)
//  }
//
//  def forAny[A](c: Constraint[A])(implicit aTT: TypeTag[A]): Constraint[List[A]] = c match {
//    case AlwaysSucceed() => success[List[A]]
//    case _ => ForAny(c)
//  }
//}
//
//trait OpticConstraintBuilder[O, A, B] {
//  def apply(o: O, c: Constraint[B]): Constraint[A]
//}
//
//object OpticConstraintBuilder {
//  implicit def prismBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Prism[A, B]), A, B] {
//    override def apply(o: (K, Prism[A, B]), c: Constraint[B]) = InPrism[A, B, K](o._1, c)(o._2)
//  }
//
//  implicit def lensBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Lens[A, B]), A, B] {
//    override def apply(o: (K, Lens[A, B]), c: Constraint[B]) = InLens[A, B, K](o._1, c)(o._2)
//  }
//
//  implicit def optionalBuilder[A, B, K](implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) = new OpticConstraintBuilder[(K, Optional[A, B]), A, B] {
//    override def apply(o: (K, Optional[A, B]), c: Constraint[B]) = InOptional[A, B, K](o._1, c)(o._2)
//  }
//
//  implicit def getterBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Getter[A, B]), A, B] {
//    override def apply(o: (K, Getter[A, B]), c: Constraint[B]) = InGetter[A, B, K](o._1, c)(o._2)
//  }
//
//  implicit def functionBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, A => B), A, B] {
//    override def apply(o: (K, A => B), c: Constraint[B]) = InGetter[A, B, K](o._1, c)(Getter(o._2))
//  }
//
//  implicit def traversalBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Traversal[A, B]), A, B] {
//    override def apply(o: (K, Traversal[A, B]),
//                       c: Constraint[B]) = InTraversal[A, B, K](o._1, c)(o._2)
//  }
//
//  implicit def traversalAllBuilder[A, B, K](implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) = new OpticConstraintBuilder[(K, Traversal[A, B]), A, List[B]] {
//
//    override def apply(o: (K, Traversal[A, B]),
//                       c: Constraint[List[B]]) = (o._1, o._2.getAll _) @: c
//  }
//}
//
//case class AlwaysSucceed[A]()(implicit aTT: TypeTag[A]) extends Constraint[A] {
//  override def apply(a: A) = a.successNel
//
//  override def not = AlwaysFail[A](None)
//
//  override def prettyPrint: String = "success"
//}
//
//
//case class AlwaysFail[A](msg: Option[String])(implicit aTT: TypeTag[A]) extends Constraint[A] {
//  override def apply(a: A) = ConstraintViolation.failure(this, a).failureNel[A]
//
//  override def not = AlwaysSucceed[A]()
//
//  override def prettyPrint: String = "failure"
//}
//
//class If[A](condition: => Constraint[A], ifTrue: => Constraint[A], ifFalse: => Constraint[A]) extends Constraint[A] {
//  override def apply(a: A) = condition(a).fold(
//    _ => ifFalse(a),
//    _ => ifTrue(a)
//  )
//
//  override def not = If(condition, ifTrue.not, ifFalse.not)
//
//  override def prettyPrint: String = s"if(${condition.prettyPrint} then ${ifTrue.prettyPrint} else ${ifFalse.prettyPrint})"
//}
//
//object If {
//  def apply[A](condition: => Constraint[A], ifTrue: => Constraint[A], ifFalse: => Constraint[A]) =
//    new If(condition, ifTrue, ifFalse)
//}
//
//case class ApplyAll[A](cs: List[Constraint[A]]) extends Constraint[A] {
//  override def apply(a: A) = {
//    val (aa, errs) = cs.foldLeft((a, List.empty[ConstraintViolation[A]]))(applyConstraint)
//
//    errs match {
//      case h::t => NonEmptyList(h, t :_*).failure[A]
//      case Nil => aa.successNel[ConstraintViolation[A]]
//    }
//  }
//
//  def applyConstraint(ae: (A, List[ConstraintViolation[A]]), c: Constraint[A]): (A, List[ConstraintViolation[A]]) =
//    c(ae._1).fold(
//      e => (ae._1, ae._2 ++ e.toList),
//      s => (s, ae._2)
//    )
//
//  override def not = ApplyAll(cs map (_.not))
//
//  override def prettyPrint: String = s"applyAll(${cs map (_.prettyPrint) mkString " "})"
//}
//
//case class ForEvery[A](c: Constraint[A])(implicit aTT: TypeTag[A]) extends Constraint[List[A]] {
//  override def apply(as: List[A]) = as match {
//    case Nil =>
//      as.successNel
//    case _ =>
//      val idx = monocle.std.list.listIndex[A]
//      (NonEmptyList.nel(as.head, IList.fromList(as.tail)).zipWithIndex map { case(a, i) =>
//        c apply a leftMap (e => e map (_.in(as, i, idx.index(i).asSetter.some))) }
//        ).sequenceU map (_.list.toList)
//  }
//
//  override def not = ForEvery(c.not)
//
//  override def prettyPrint: String = s"forEvery(${c.prettyPrint})"
//}
//
//case class ForAny[A](c: Constraint[A])(implicit aTT: TypeTag[A]) extends Constraint[List[A]] {
//  override def apply(as: List[A]) = as match {
//    case Nil =>
//      as.successNel
//    case _ => // fixme: we're loosing logging here
//      val idx = monocle.std.list.listIndex[A]
//      val apps = as map c.apply
//      apps.filter(_.isSuccess) headOption match {
//        case Some(s) =>
//          as.successNel
//        case None =>
//          ConstraintViolation.failure(this, as).failureNel
//      }
//  }
//
//  override def not = ForAny(c.not)
//
//  override def prettyPrint = s"forAny(${c.prettyPrint}"
//}
//
//case class EqualTo[A](eA: A)(implicit aTT: TypeTag[A]) extends Constraint[A] {
//  override def apply(a: A) =
//    if(eA == a) a.successNel else ConstraintViolation.failure(this, a).failureNel
//
//  override def not = NotEqualTo(eA)
//
//  override def prettyPrint: String = s"($eA == _)"
//}
//
//case class NotEqualTo[A](eA: A)(implicit aTT: TypeTag[A]) extends Constraint[A] {
//  override def apply(a: A) =
//    if(eA == a) ConstraintViolation.failure(this, a).failureNel else a.successNel
//
//  override def not = EqualTo(eA)
//
//  override def prettyPrint: String = s"($eA != _)"
//}
//
case class SetContainsMember[A](a: A) extends Constraint[Set[A]] {
  override type Path = HNil

  override def apply(as: Set[A]) =
    if(as contains a) Nil else ConstraintViolation(as, this) :: Nil
//
//  override def not = NotMemberOf(a)

  override def prettyPrint: StringBuilder = new StringBuilder() append "set contains " append a
}
//
case class NotMemberOf[A](a: A)(implicit aTT: TypeTag[A]) extends Constraint[Set[A]] {
  override type Path = HNil

  override def apply(as: Set[A]) =
    if(as contains a) ConstraintViolation(as, this) :: Nil else Nil

//  override def not = MemberOf(a)

  override def prettyPrint: StringBuilder = new StringBuilder() append "set doesn't contain " append a
}

case class LessThan(min: Int) extends Constraint[Int] {
  override type Path = HNil

  override def apply(a: Int) =
    if(a < min) Nil else ConstraintViolation(a, this) :: Nil

//  override def not = NotLessThan(min)

  override def prettyPrint: StringBuilder = new StringBuilder() append "_ < " append min
}

case class NotLessThan(min: Int) extends Constraint[Int] {
  override type Path = HNil

  override def apply(a: Int) =
    if(a < min) ConstraintViolation(a, this) :: Nil else Nil

//  override def not = LessThan(min)

  override def prettyPrint: StringBuilder = new StringBuilder() append "_ >= " append min
}

case class GreaterThan[A](max: Int) extends Constraint[Int] {
  override type Path = HNil

  override def apply(a: Int) =
    if(a > max) Nil else ConstraintViolation(a, this) :: Nil

//  override def not = NotGreaterThan(max)

  override def prettyPrint: StringBuilder = new StringBuilder() append "_ > " append max
}

case class NotGreaterThan[A](max: Int) extends Constraint[Int] {
  override type Path = HNil

  override def apply(a: Int) =
    if(a > max) ConstraintViolation(a, this) :: Nil else Nil

//  override def not = GreaterThan(max)

  override def prettyPrint: StringBuilder = new StringBuilder() append "_ <= " append max
}
//
//case class InPrism[A, B, K](key: K, bC: Constraint[B])
//                           (prism: Prism[A, B])
//                           (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
//  override def apply(a: A) =
//    prism.getOption(a) match {
//      case Some(b) =>
//        bC(b).leftMap(_.map(_.in(a, key, prism.asSetter.some))).map(b => prism.set(b)(a))
//      case None =>
//        a.successNel
//    }
//
//
//  override def not = InPrism(key, bC.not)(prism)
//
//  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
//}
//
//case class InLens[A, B, K](key: K, bC: Constraint[B])
//                          (lens: Lens[A, B])
//                          (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
//  override def apply(a: A) =
//    bC(lens.get(a)).leftMap(_.map(_.in(a, key, lens.asSetter.some))).map(b => lens.set(b)(a))
//
//
//  override def not = InLens(key, bC.not)(lens)
//
//  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
//}
//
//case class InOptional[A, B, K](key: K, bC: Constraint[B])
//                              (optional: Optional[A, B])
//                              (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
//  override def apply(a: A) =
//    optional.getOption(a) match {
//      case Some(b) =>
//        bC(b).leftMap(_.map(_.in(a, key, optional.asSetter.some))).map(b => optional.set(b)(a))
//      case None =>
//        a.successNel
//    }
//
//  override def not = InOptional(key, bC.not)(optional)
//
//  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
//}
//
//case class InTraversal[A, B, K](key: K, bC: Constraint[B])
//                               (traversal: Traversal[A, B])
//                               (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
//  override def apply(a: A) =
//    traversal.getAll(a) match {
//      case Nil =>
//        a.successNel
//      case bs =>
//        (NonEmptyList(bs.head, bs.tail : _*) map { b =>
//                bC apply b leftMap (e => e map (_.in(a, b, (traversal composePrism optics.unsafeSelect((_: B) == b)).asSetter.some))) }
//                ).sequenceU map (_ => a) // nasty - dropping the modified b's on the floor
//    }
//
//
//  override def not = InTraversal(key, bC.not)(traversal)
//
//  override def prettyPrint = s"at($key -> ${bC.prettyPrint}"
//}
//
//case class InGetter[A, B, K](key: K, bC: Constraint[B])
//                            (getter: Getter[A, B])
//                            (implicit aTpe: TypeTag[A], kTpe: TypeTag[K], bTpe: TypeTag[B]) extends Constraint[A] {
//  override def apply(a: A) =
//    bC(getter.get(a)).leftMap(_.map(_.in(a, key, none))).map(_ => a)
//
//
//  override def not = InGetter(key, bC.not)(getter)
//
//  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
//}
//
trait ConstraintSystem {

  type Report

  def validate(ctxt: EvalContext): InstanceExp => Report

}

//object ConstraintSystem {
//  def apply(cs: ConstraintSystem*) = new {
//    def apply(recovery: ConstraintRecovery[_, _]*) = new {
//      def apply(ctxt: EvalContext) = new Constraint[SBEvaluatedFile] {
//        val allConstraints = Constraint.applyAll(cs.to[List] map (_.fromContext(ctxt)))
//
//        val recoveringConstraint = new Constraint[TopLevel.InstanceExp] {
//          override def apply(a: TopLevel.InstanceExp): ValidatedConstraints[TopLevel.InstanceExp] =
//            applyWithRecovery(a).validationNel
//
//          def applyWithRecovery(a: TopLevel.InstanceExp): ConstraintViolation[TopLevel.InstanceExp] \/ TopLevel.InstanceExp =
//            allConstraints(a).fold(
//                        err => {
//                          println(s"*** applyWithRecovery *** at $a for error ${err.head.prettyPrint}")
//                          val ar = attemptRecovery(err.head)
//                          println(s"*** recovered *** to ${ar.fold(_.prettyPrint, _.toString)}")
//                          val aar = ar flatMap applyWithRecovery
//                          println(s"*** finally *** $aar")
//                          aar
//                        },
//                        \/-(_)
//                      )
//
//          def attemptRecovery(cv: ConstraintViolation[TopLevel.InstanceExp]): ConstraintViolation[TopLevel.InstanceExp] \/ TopLevel.InstanceExp =
//            recovery.flatMap(r =>
//              // type hack -- should do better
//              cv recoverWith r.asInstanceOf[ConstraintRecovery[ConstraintViolation[Any], Any]] flatMap { case(None, tie) =>
//                println(s"Overseen recovery to: $tie")
//                val (c,t) = tie.eval.run(ctxt)
//                println("Log messages:")
//                println(c.logms.drop(ctxt.logms.length).map(_.pretty).mkString("\n"))
//                t.headOption
//              }
//            ).headOption \/> cv
//
//
//          override def not = ???
//
//          override def prettyPrint = allConstraints.prettyPrint
//        } log "recoveringConstraint"
//
//        val sbEFConstraint = (
//          ('tops, optics.sbEvaluatedFile.tops) @:
//          (ForEvery(
//            recoveringConstraint
//          ) log "tops")
//          ) log "sbEFConstraint"
//
//        def apply(sev: SBEvaluatedFile) = sbEFConstraint(sev)
//
//        override def not = ???
//
//        override def prettyPrint = sbEFConstraint.prettyPrint
//      }
//
//      def apply(cf : (EvalContext, SBEvaluatedFile))
//      : ValidatedConstraints[SBEvaluatedFile] = apply(cf._1)(cf._2)
//    }
//  }
//}





