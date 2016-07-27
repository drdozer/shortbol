package uk.co.turingatemyhamster.shortbol
package ops

import scala.reflect.runtime.universe.TypeTag

import ast._
import sugar._

import scalaz.{IList, NonEmptyList, Scalaz, Validation, ValidationNel, \/}
import Scalaz._
import monocle._
import monocle.macros._


sealed trait ConstraintViolation[A] {
  def at: A

  def in[X, K](in: X, key: K, setter: Option[Setter[X, A]])
              (implicit aTpe: TypeTag[X], kTpe: TypeTag[K], bTpe: TypeTag[A]): ConstraintViolation[X] =
    NestedViolation[X, K, A](in, key, this)(setter)

  def prettyPrint: String

  def recoverWith[C <: ConstraintViolation[X], X](f: C => Option[X])(implicit cTT: TypeTag[C], xTT: TypeTag[X]): Option[A]
}

object ConstraintViolation {
  def failure[A](rule: Constraint[A], at: A)(implicit aTT: TypeTag[A]): ConstraintViolation[A] =
    ConstraintFailure(rule, at)
}

case class ConstraintFailure[A](rule: Constraint[A], at: A)
                               (implicit cfaTT: TypeTag[ConstraintFailure[A]], aTT: TypeTag[A]) extends ConstraintViolation[A] {
  override def prettyPrint: String = s"failed(${rule.prettyPrint} at $at"

  override def recoverWith[C <: ConstraintViolation[X], X](f: C => Option[X])
                                                          (implicit cTT: TypeTag[C], xTT: TypeTag[X]) = {
    if(cfaTT.tpe <:< cTT.tpe && aTT.tpe <:< xTT.tpe) {
      f(this.asInstanceOf[C]).map(_.asInstanceOf[A])
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

  override def recoverWith[C <: ConstraintViolation[X], X](f: C => Option[X])
                                                          (implicit cTT: TypeTag[C], xTT: TypeTag[X]) = {
    if(nvTT.tpe <:< cTT.tpe && aTT.tpe <:< xTT.tpe) {
      f(this.asInstanceOf[C]).map(_.asInstanceOf[A])
    } else {
      for {
        s <- setter
        b <- because.recoverWith(f)
      } yield s.set(b)(at)
    }
  }
}

trait Constraint[A] {
  def apply(a: A): ValidationNel[ConstraintViolation[A], A]

  def onlyIf(c: Constraint[A]) = If(c, this, Constraint.success)
  def unless(c: Constraint[A]) = If(c, Constraint.success, this)

  def requires(c: Constraint[A])(implicit aTT: TypeTag[A]) = If(c, this, Constraint.fail[A])
  def rejecting(c: Constraint[A])(implicit aTT: TypeTag[A]) = If(c, Constraint.fail[A], this)


  def @: [O, X](o: O)(implicit ocb: OpticConstraintBuilder[O, X, A]): Constraint[X] = this match {
    case AlwaysSucceed() => Constraint.success[X]
    case c => ocb(o, c)
  }

  def prettyPrint: String
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
}

object Constraint {

  type CheckedConstraint[A] = Validation[ConstraintViolation[A], A]
  type CheckedConstraints[A] = ValidationNel[ConstraintViolation[A], A]

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
  override def apply(a: A) =
    (NonEmptyList.nel(cs.head, IList.fromList(cs.tail)) map (_ apply a)).sequenceU map (_.head)

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
        bC(b).leftMap(_.map(_.in(a, key, prism.asSetter.some))).map(_ => a)
      case None =>
        a.successNel
    }

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class InLens[A, B, K](key: K, bC: Constraint[B])
                          (lens: Lens[A, B])
                          (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    bC(lens.get(a)).leftMap(_.map(_.in(a, key, lens.asSetter.some))).map(_ => a)

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class InOptional[A, B, K](key: K, bC: Constraint[B])
                              (optional: Optional[A, B])
                              (implicit aTT: TypeTag[A], kTT: TypeTag[K], bTT: TypeTag[B]) extends Constraint[A] {
  override def apply(a: A) =
    optional.getOption(a) match {
      case Some(b) =>
        bC(b).leftMap(_.map(_.in(a, key, optional.asSetter.some))).map(_ => a)
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
  def fromContext(ctxt: EvalContext): Constraint[SBEvaluatedFile]

  final def apply(cf: (EvalContext, SBEvaluatedFile)) =
    fromContext(cf._1).apply(cf._2)
}

object ConstraintSystem {
  def apply(cs: ConstraintSystem*) = new ConstraintSystem {
    override def fromContext(ctxt: EvalContext) =
      Constraint.applyAll(cs.to[List] map (_.fromContext(ctxt)))
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
  }

  object bodyStmt {
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
}

object OWL extends ConstraintSystem {
  val owl = "owl" : NSPrefix
  val owl_class = owl :# "Class"
  val owl_propertyRestriction = owl :# "propertyRestriction"
  val owl_minCardinality = owl :# "minCardinality"
  val owl_maxCardinality = owl :# "maxCardinality"
  val owl_exactCardinality = owl :# "exactCardinality"
  val owl_allValuesFrom = owl :# "allValuesFrom"
  val owl_subClassOf = owl :# "subClassOf"

  trait Typer[A] {
    def exactTypeOf(a: A): Set[Identifier]
  }

  implicit val instanceExpTyper: Typer[InstanceExp] = new Typer[InstanceExp] {
    override def exactTypeOf(a: InstanceExp) = a match {
      case InstanceExp(_, ConstructorApp(TpeConstructor1(t, _), _)) => Set(t)
      case _ => Set()
    }
  }

  implicit val literalTyper: Typer[Literal] = new Typer[Literal] {
    override def exactTypeOf(a: Literal) = a match {
      case IntegerLiteral(_) => Set("xsd":#"integer")
      case StringLiteral(_, dt, _) => dt map (d => Set(d.tpe)) getOrElse Set("xsd":#"string")
    }
  }

  def bySize[T](c: Constraint[Int])(implicit tTpe: TypeTag[T]) = ('size, Getter((_:List[T]).size)) @: c

  def minCardinalityConstraint(propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_minCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotLessThan(c)).some
      case _ =>
        none
    }

  def maxCardinalityConstraint(propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_maxCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotGreaterThan(c)).some
      case _ =>
        none
    }

  def exactCardinalityConstraint(propRes: BodyStmt): List[Constraint[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_exactCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotLessThan(c)) :: bySize[BodyStmt](NotGreaterThan(c)) :: Nil
      case _ =>
        Nil
    }


  class ContextConstraints(ctxt: EvalContext) extends Constraint[SBEvaluatedFile] {

    implicit val identifierTyper: Typer[Identifier] = new Typer[Identifier] {
      override def exactTypeOf(a: Identifier) =
        for {
          eqA <- equIds.getOrElse(a, Set(a))
          is <- ctxt.insts.get(eqA).to[Set]
          i <- is
          t <- implicitly[Typer[InstanceExp]] exactTypeOf i
        } yield t
    }


    def byType[A](tpe: Identifier)(implicit ty: Typer[A], aTpe: TypeTag[A]) =
      ('type, Getter((a: A) => {
        ty exactTypeOf a flatMap (ta =>
          flatHierarchy.getOrElse(ta, Set(ta)))
      })) @: MemberOf(tpe)


    def allValuesFromConstraint(propRes: BodyStmt): Option[Constraint[BodyStmt]] =
      propRes match {
        case BodyStmt.Assignment(Assignment(`owl_allValuesFrom`, ValueExp.Identifier(tpe))) =>
          Constraint.applyAll(
            List(
              ('instance, optics.bodyStmt.instanceExp) @: byType[InstanceExp](tpe),
              ('assignment, optics.bodyStmt.assignment) @:
                ('value, optics.assignment.value) @: Constraint.applyAll(
                List(
                  ('identifier, optics.valueExp.identifier) @: byType[Identifier](tpe),
                  ('literal, optics.valueExp.literal) @: byType[Literal](tpe)
                )
              )
            )
          ).some
        case _ =>
          none
      }

    def restrictions(restrs: Seq[BodyStmt]): Constraint[List[BodyStmt]] =
      Constraint.applyAll(
        restrs.to[List] flatMap { r =>
          List.empty ++
            minCardinalityConstraint(r) ++
            maxCardinalityConstraint(r) ++
            exactCardinalityConstraint(r) ++
            (allValuesFromConstraint(r).to[List] map Constraint.forEvery)
        }
      )

    def restrictionInstance(i: InstanceExp): Option[Constraint[List[BodyStmt]]] = i match {
      case InstanceExp(propId, ConstructorApp(TpeConstructor1(`owl_propertyRestriction`, _), restrs)) =>
        ((propId, Getter((_: List[BodyStmt]) collect {
          case b@BodyStmt.Assignment(Assignment(p, _)) if p == propId => b : BodyStmt
          case b@BodyStmt.InstanceExp(InstanceExp(p, _)) if p == propId => b : BodyStmt
        })) @: restrictions(restrs)).some
      case _ => None
    }

    def owlClassConstraint(cl: InstanceExp): Option[Constraint[InstanceExp]] =
      cl match {
        case InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), clsBdy)) =>
          val rs = Constraint.applyAll(
            clsBdy.to[List].flatMap {
              case BodyStmt.InstanceExp(i) => restrictionInstance(i)
              case _ => Nil
            })
          ((('properties, optics.instanceExp.cstrApp composeLens optics.constructorApp.body) @: rs) onlyIf
            byType[InstanceExp](clsId)).some
        case _ =>
          none
      }

    val equIds = {
      var clusters = Map.empty[Identifier, Set[Identifier]]

      for {
        (i, js) <- ctxt.vlxps
        _ = if(js.size != 1) throw new IllegalStateException(s"Expected one value for $i but found $js")
        ValueExp.Identifier(j) <- js
      } {
        val iS = clusters.getOrElse(i, Set(i))
        val jS = clusters.getOrElse(j, Set(j))
        val ijS = iS ++ jS
        clusters += (i -> ijS)
        clusters += (j -> ijS)
      }

      clusters
    }

    val allClassIds = for {
          is <- ctxt.insts.values.to[List]
          _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
          InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), _)) <- is
    } yield clsId

    val classHierarchy = (for {
      is <- ctxt.insts.values.to[List]
      _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
      InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), clsBdy)) <- is
      BodyStmt.Assignment(Assignment(`owl_subClassOf`, ValueExp.Identifier(superType))) <- clsBdy
    } yield clsId -> superType)
      .foldLeft(Map.empty[Identifier, List[Identifier]])((m, ii) => m + (ii._1 -> (ii._2 :: m.getOrElse(ii._1, Nil))))

    val flatHierarchy = {
      val cache = scala.collection.mutable.Map.empty[Identifier, Set[Identifier]]

      def unwrap(i: Identifier): Set[Identifier] = cache.getOrElseUpdate(
        i,
        Set(i) ++ (classHierarchy.getOrElse(i, Nil) flatMap unwrap))

      allClassIds foreach unwrap

      Map(cache.to[Seq] :_*)
    }

    val allOwlClasses = (for {
      is <- ctxt.insts.values.to[List]
      _ = if(is.size > 1) throw new IllegalStateException(s"Multiple definitions for ${is.head.id}")
      i <- is
      c <- owlClassConstraint(i)
    } yield i.id -> c).toMap

    val flatClasses = flatHierarchy.values map (tps => Constraint.applyAll(tps.to[List] map allOwlClasses))

    val tcInstanceExp = Constraint.applyAll(flatClasses.to[List])

    val tcTopLevel = ('instanceExp, optics.topLevel.instanceExp.instanceExp) @: tcInstanceExp

    val tcAllTops: Constraint[List[TopLevel.InstanceExp]] = ForEvery(tcTopLevel)

    val tcSBFile: Constraint[SBEvaluatedFile] = ('tops, optics.sbEvaluatedFile.tops) @: tcAllTops

    override def apply(a: SBEvaluatedFile) =
      tcSBFile apply a

    override def prettyPrint: String = tcSBFile.prettyPrint
  }

  override def fromContext(ctxt: EvalContext): ContextConstraints = new ContextConstraints(ctxt)
}

