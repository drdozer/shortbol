package uk.co.turingatemyhamster.shortbol
package ops

import ast._
import sugar._

import scalaz._
import Scalaz._

trait ConstraintViolation[A] {
  def at[X, K](in: X, at: K): ConstraintViolation[X] = ViolationAt[X, K, A](in, at, this)
}

object ConstraintViolation {
  def failure[A](rule: ConstraintRule[A], at: A): ConstraintViolation[A] =
    ConstraintFailure(rule, at)
}

case class ConstraintFailure[A](rule: ConstraintRule[A], at: A) extends ConstraintViolation[A]
case class ViolationAt[A, K, B](in: A, at: K, because: ConstraintViolation[B]) extends ConstraintViolation[A]

trait ConstraintRule[A] {
  def apply(a: A): ValidationNel[ConstraintViolation[A], A]

  def onlyIf(c: ConstraintRule[A]) = If(c, this, ConstraintRule.success)
  def unless(c: ConstraintRule[A]) = If(c, ConstraintRule.success, this)

  def requires(c: ConstraintRule[A]) = If(c, this, ConstraintRule.fail)
  def rejecting(c: ConstraintRule[A]) = If(c, ConstraintRule.fail, this)
}

object ConstraintRule {

  type CheckedConstraint[A] = Validation[ConstraintViolation[A], A]
  type CheckedConstraints[A] = ValidationNel[ConstraintViolation[A], A]

  def success[A] = AlwaysSucceed[A]()
  def fail[A] = AlwaysFail[A](none)


  def applyAll[A](cs: List[ConstraintRule[A]]): ConstraintRule[A] =
    cs flatMap {
      case AlwaysSucceed() => Nil
      case ApplyAll(xx) => xx
      case x => x :: Nil
    } match {
      case Nil => success[A]
      case h::Nil => h
      case css => ApplyAll(css)
    }

  def forEvery[A](c: ConstraintRule[A]): ConstraintRule[List[A]] =
    c match {
      case AlwaysSucceed() => success[List[A]]
      case _ => ForEvery(c)
    }
}

case class AlwaysSucceed[A]() extends ConstraintRule[A] {
  override def apply(a: A) = a.successNel
}


case class AlwaysFail[A](msg: Option[String]) extends ConstraintRule[A] {
  override def apply(a: A) = ConstraintViolation.failure(this, a).failureNel[A]
}

case class If[A](condition: ConstraintRule[A], ifTrue: ConstraintRule[A], ifFalse: ConstraintRule[A]) extends ConstraintRule[A] {
  override def apply(a: A) = condition(a).fold(
    _ => ifFalse(a),
    _ => ifTrue(a)
  )
}

case class ApplyAll[A](cs: List[ConstraintRule[A]]) extends ConstraintRule[A] {
  override def apply(a: A) =
    (NonEmptyList.nel(cs.head, IList.fromList(cs.tail)) map (_ apply a)).sequenceU map (_.head)
}

case class ForEvery[A](c: ConstraintRule[A]) extends ConstraintRule[List[A]] {
  override def apply(as: List[A]) = as match {
    case Nil =>
      as.successNel
    case _ =>
      (NonEmptyList.nel(as.head, IList.fromList(as.tail)).zipWithIndex map { case(a, i) =>
        c apply a leftMap (e => e map (_.at(as, i))) }
        ).sequenceU map (_.list.toList)
  }
}

case class EqualTo[A](eA: A) extends ConstraintRule[A] {
  override def apply(a: A) =
    if(eA == a) a.successNel else ConstraintViolation.failure(this, a).failureNel
}

case class In[A](a: A) extends ConstraintRule[Set[A]] {
  override def apply(as: Set[A]) =
    if(as contains a) as.successNel else ConstraintViolation.failure(this, as).failureNel
}

case class NotLessThan[A](min: Int) extends ConstraintRule[Int] {
  override def apply(a: Int) =
    if(a < min) ConstraintViolation.failure(this, a).failureNel else a.successNel
}

case class NotGreaterThan[A](max: Int) extends ConstraintRule[Int] {
  override def apply(a: Int) =
    if(a > max) ConstraintViolation.failure(this, a).failureNel else a.successNel
}

case class At[A, B](key: Symbol, bC: ConstraintRule[B])(f: A => B) extends ConstraintRule[A] {
  override def apply(a: A) =
    bC(f(a)).leftMap(_.map(_.at(a, key))).map(_ => a)
}

object At {
  def from[B](key: Symbol, bc: ConstraintRule[B]) = new {
    def following[A](f: A => B): ConstraintRule[A] = bc match {
      case AlwaysSucceed() => ConstraintRule.success[A]
      case _ => At[A, B](key, bc)(f)
    }
  }
}

trait ConstraintSystem {
  def fromContext(ctxt: EvalContext): ConstraintRule[SBEvaluatedFile]

  final def apply(cf: (EvalContext, SBEvaluatedFile)) =
    fromContext(cf._1).apply(cf._2)
}

object ConstraintSystem {
  def apply(cs: ConstraintSystem*) = new ConstraintSystem {
    override def fromContext(ctxt: EvalContext) =
      ConstraintRule.applyAll(cs.to[List] map (_.fromContext(ctxt)))
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
    def exactTypeOf(a: A): Option[Identifier]
  }

  implicit val instanceExpTyper: Typer[InstanceExp] = new Typer[InstanceExp] {
    override def exactTypeOf(a: InstanceExp) = a match {
      case InstanceExp(_, ConstructorApp(TpeConstructor1(t, _), _)) => t.some
      case _ => none
    }
  }

  implicit val literalTyper: Typer[Literal] = new Typer[Literal] {
    override def exactTypeOf(a: Literal) = a match {
      case IntegerLiteral(_) => Some("xsd":#"integer")
      case StringLiteral(_, dt, _) => dt map (_.tpe) orElse ("xsd":#"string").some
    }
  }

  def bySize[T](c: ConstraintRule[Int]) = At.from('size, c) following ((_:List[T]).size)

  def minCardinalityConstraint(propRes: BodyStmt): Option[ConstraintRule[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_minCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotLessThan(c)).some
      case _ =>
        none
    }

  def maxCardinalityConstraint(propRes: BodyStmt): Option[ConstraintRule[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_maxCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotGreaterThan(c)).some
      case _ =>
        none
    }

  def exactCardinalityConstraint(propRes: BodyStmt): List[ConstraintRule[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_exactCardinality`, ValueExp.Literal(IntegerLiteral(c)))) =>
        bySize[BodyStmt](NotLessThan(c)) :: bySize[BodyStmt](NotGreaterThan(c)) :: Nil
      case _ =>
        Nil
    }


  class ContextConstraints(ctxt: EvalContext) extends ConstraintRule[SBEvaluatedFile] {

    implicit val identifierTyper: Typer[Identifier] = new Typer[Identifier] {
      override def exactTypeOf(a: Identifier) =
        for {
          is <- ctxt.insts.get(a)
          i <- is.headOption
          t <- implicitly[Typer[InstanceExp]] exactTypeOf i
        } yield t
    }

    implicit val assignmentTyper: Typer[Assignment] = new Typer[Assignment] {
      override def exactTypeOf(a: Assignment) = a.value match {
        case ValueExp.Identifier(i) => implicitly[Typer[Identifier]] exactTypeOf i
        case ValueExp.Literal(l) => implicitly[Typer[Literal]] exactTypeOf l
      }
    }

    implicit val bsTyper: Typer[BodyStmt] = new Typer[BodyStmt] {
      override def exactTypeOf(b: BodyStmt) = b match {
        case BodyStmt.InstanceExp(i) =>
          implicitly[Typer[InstanceExp]] exactTypeOf i
        case BodyStmt.Assignment(a) =>
          implicitly[Typer[Assignment]] exactTypeOf a
        case _ => None
      }
    }

    def byType[A](tpe: Identifier)(implicit ty: Typer[A]) =
      At.from('type, In(tpe)) following ((a: A) => {
        ty exactTypeOf a map (ta =>
          flatHierarchy.getOrElse(ta, Set(ta))) getOrElse Set.empty
      })


    def allValuesFromConstraint(propRes: BodyStmt): Option[ConstraintRule[List[BodyStmt]]] =
      propRes match {
        case BodyStmt.Assignment(Assignment(`owl_allValuesFrom`, ValueExp.Identifier(tpe))) =>
          ConstraintRule.forEvery(
            byType[BodyStmt](tpe)).some
        case _ =>
          none
      }

    def restrictions(restrs: Seq[BodyStmt]): ConstraintRule[List[BodyStmt]] =
      ConstraintRule.applyAll(
        restrs.to[List] flatMap { r =>
          List.empty ++
            minCardinalityConstraint(r) ++
            maxCardinalityConstraint(r) ++
            exactCardinalityConstraint(r) ++
            allValuesFromConstraint(r)
        }
      )

    def restrictionInstance(i: InstanceExp): Option[ConstraintRule[List[BodyStmt]]] = i match {
      case InstanceExp(propId, ConstructorApp(TpeConstructor1(`owl_propertyRestriction`, _), restrs)) =>
        (At.from(Symbol(s"property($propId)"), restrictions(restrs)) following ((_: List[BodyStmt]) collect {
          case b@BodyStmt.Assignment(Assignment(p, _)) if p == propId => b
          case b@BodyStmt.InstanceExp(InstanceExp(p, _)) if p == propId => b
        })).some
      case _ => None
    }

    def owlClassConstraint(cl: InstanceExp): Option[ConstraintRule[InstanceExp]] =
      cl match {
        case InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), clsBdy)) =>
          val rs = ConstraintRule.applyAll(
            clsBdy.to[List].flatMap {
              case BodyStmt.InstanceExp(i) => restrictionInstance(i)
              case _ => Nil
            })
          (At.from('properties, rs) following ((_:InstanceExp).cstrApp.body.to[List]) onlyIf byType[InstanceExp](clsId)).some
        case _ =>
          none
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

    val flatClasses = flatHierarchy.values map (tps => ConstraintRule.applyAll(tps.to[List] map allOwlClasses))

    val allContext =
      At.from('instances, ConstraintRule.forEvery(ConstraintRule.applyAll(flatClasses.to[List]))) following ((_: SBEvaluatedFile).tops.to[List] collect {
        case TopLevel.InstanceExp(i) =>
          i
      })

    override def apply(a: SBEvaluatedFile) =
      allContext apply a

    override def toString = allContext.toString
  }

  override def fromContext(ctxt: EvalContext): ContextConstraints = new ContextConstraints(ctxt)
}

