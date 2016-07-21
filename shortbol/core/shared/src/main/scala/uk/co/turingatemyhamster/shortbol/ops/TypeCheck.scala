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

case class AndOf[A](cs: List[ConstraintRule[A]]) extends ConstraintRule[A] {
  override def apply(a: A) =
    (NonEmptyList.nel(cs.head, IList.fromList(cs.tail)) map (_ apply a)).sequenceU map (_.head)
}

case class ForEvery[A](c: ConstraintRule[A]) extends ConstraintRule[List[A]] {
  override def apply(as: List[A]) =
    (NonEmptyList.nel(as.head, IList.fromList(as.tail)).zipWithIndex map { case(a, i) =>
      c apply a leftMap (e => e map (_.at(as, i))) }
      ).sequenceU map (_.list.toList)
}

case class EqualTo[A](eA: A) extends ConstraintRule[A] {
  override def apply(a: A) =
    if(eA == a) a.successNel else ConstraintViolation.failure(this, a).failureNel
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
    def following[A](f: A => B): ConstraintRule[A] = At[A, B](key, bc)(f)
  }
}


//
//case class InstanceHasWellFormedProperties(pc: ConstraintRule[List[BodyStmt]]) extends ConstraintRule[InstanceExp]
//
//case class PropertyValuesAreWellFormed(prop: Identifier, vc: ConstraintRule[List[BodyStmt]]) extends ConstraintRule[List[BodyStmt]]
//

object OwlConstraints {
  val owl = "owl" : NSPrefix
  val owl_class = owl :# "class"
  val owl_restriction = owl :# "restriction"
  val owl_minCardinality = owl :# "minCardinality"
  val owl_maxCardinality = owl :# "maxCardinality"
  val owl_exactCardinality = owl :# "exactCardinality"
  val owl_allValuesFrom = owl :# "allValuesFrom"
  val owl_subClassOf = owl :# "subClassOf"

  trait Typer[A] {
    def apply(a: A): Option[Identifier]
  }

  implicit val bsTyper: Typer[BodyStmt] = new Typer[BodyStmt] {
    override def apply(a: BodyStmt) = a match {
      case BodyStmt.InstanceExp(i) =>
        implicitly[Typer[InstanceExp]] apply i
      case _ => None
    }
  }

  implicit val instTyper: Typer[InstanceExp] = new Typer[InstanceExp] {
    override def apply(a: InstanceExp) = a match {
      case InstanceExp(_, ConstructorApp(TpeConstructor1(t, _), _)) => t.some
      case _ => none
    }
  }

  def byType[A](tpe: Identifier)(implicit t: Typer[A]) =
    At.from('type, EqualTo(tpe.some)) following t.apply

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

  def allValuesFromConstraint(propRes: BodyStmt): Option[ConstraintRule[List[BodyStmt]]] =
    propRes match {
      case BodyStmt.Assignment(Assignment(`owl_allValuesFrom`, ValueExp.Identifier(tpe))) =>
        ForEvery(
          byType[BodyStmt](tpe)).some
      case _ =>
        none
    }

  def restrictions(restrs: Seq[BodyStmt]): ConstraintRule[List[BodyStmt]] =
    AndOf(
      restrs.to[List] flatMap { r =>
        List.empty ++
          minCardinalityConstraint(r) ++
          maxCardinalityConstraint(r) ++
          exactCardinalityConstraint(r) ++
          allValuesFromConstraint(r)
      }
    )

  def owlClassConstraint(cl: TopLevel.InstanceExp): Option[ConstraintRule[InstanceExp]] =
    cl match {
      case TopLevel.InstanceExp(InstanceExp(clsId, ConstructorApp(TpeConstructor1(`owl_class`, _), clsBdy))) =>
        val rs = AndOf(
          clsBdy.to[List].collect {
            case BodyStmt.InstanceExp(InstanceExp(propId, ConstructorApp(TpeConstructor1(`owl_restriction`, _), restrs))) =>
              At.from(Symbol(s"property($propId)"), restrictions(restrs)) following ((_: List[BodyStmt]) collect {
                case b@BodyStmt.Assignment(Assignment(p, _)) if p == propId => b
                case b@BodyStmt.InstanceExp(InstanceExp(p, _)) if p == propId => b
              })
          })
        (At.from('properties, rs) following ((_:InstanceExp).cstrApp.body.to[List]) onlyIf byType[InstanceExp](clsId)).some
      case _ =>
        none
    }
}
