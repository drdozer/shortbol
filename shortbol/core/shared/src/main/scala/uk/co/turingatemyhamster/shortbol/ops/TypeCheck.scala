package uk.co.turingatemyhamster.shortbol
package ops

import ast._
import sugar._

import scalaz._
import Scalaz._

trait ConstraintViolation[A] {
  def at[X, K](in: X, at: K): ConstraintViolation[X] = ViolationAt[X, K, A](in, at, this)

  def prettyPrint: String
}

object ConstraintViolation {
  def failure[A](rule: Constraint[A], at: A): ConstraintViolation[A] =
    ConstraintFailure(rule, at)
}

case class ConstraintFailure[A](rule: Constraint[A], at: A) extends ConstraintViolation[A] {
  override def prettyPrint: String = s"failed(${rule.prettyPrint} at $at"
}

case class ViolationAt[A, K, B](in: A, at: K, because: ConstraintViolation[B]) extends ConstraintViolation[A] {
  def prettyIn: String = {
    val s = in.toString
    if(s.length > 40) s.substring(0, 36) + " ..." else s
  }
  override def prettyPrint: String = s"violation($at of $prettyIn because ${because.prettyPrint})"
}

trait Constraint[A] {
  def apply(a: A): ValidationNel[ConstraintViolation[A], A]

  def onlyIf(c: Constraint[A]) = If(c, this, Constraint.success)
  def unless(c: Constraint[A]) = If(c, Constraint.success, this)

  def requires(c: Constraint[A]) = If(c, this, Constraint.fail)
  def rejecting(c: Constraint[A]) = If(c, Constraint.fail, this)

  def prettyPrint: String
}

object Constraint {

  type CheckedConstraint[A] = Validation[ConstraintViolation[A], A]
  type CheckedConstraints[A] = ValidationNel[ConstraintViolation[A], A]

  def success[A] = AlwaysSucceed[A]()
  def fail[A] = AlwaysFail[A](none)


  def applyAll[A](cs: List[Constraint[A]]): Constraint[A] =
    cs flatMap {
      case AlwaysSucceed() => Nil
      case ApplyAll(xx) => xx
      case x => x :: Nil
    } match {
      case Nil => success[A]
      case h::Nil => h
      case css => ApplyAll(css)
    }

  def forEvery[A](c: Constraint[A]): Constraint[List[A]] =
    c match {
      case AlwaysSucceed() => success[List[A]]
      case _ => ForEvery(c)
    }

  def at[B, K](key: K, bc: Constraint[B]) = new {
    def following[A](f: A => B): Constraint[A] = bc match {
      case AlwaysSucceed() => Constraint.success[A]
      case _ => At[A, B, K](key, bc)(f)
    }
  }

  def switch[A](default: Constraint[A])(fc: PartialFunction[A, Constraint[A]]) =
    Switch(default)(fc)
}

case class AlwaysSucceed[A]() extends Constraint[A] {
  override def apply(a: A) = a.successNel

  override def prettyPrint: String = "success"
}


case class AlwaysFail[A](msg: Option[String]) extends Constraint[A] {
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

case class ForEvery[A](c: Constraint[A]) extends Constraint[List[A]] {
  override def apply(as: List[A]) = as match {
    case Nil =>
      as.successNel
    case _ =>
      (NonEmptyList.nel(as.head, IList.fromList(as.tail)).zipWithIndex map { case(a, i) =>
        c apply a leftMap (e => e map (_.at(as, i))) }
        ).sequenceU map (_.list.toList)
  }

  override def prettyPrint: String = s"forEvery(${c.prettyPrint})"
}

case class EqualTo[A](eA: A) extends Constraint[A] {
  override def apply(a: A) =
    if(eA == a) a.successNel else ConstraintViolation.failure(this, a).failureNel

  override def prettyPrint: String = s"($eA == _)"
}

case class In[A](a: A) extends Constraint[Set[A]] {
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

case class At[A, B, K](key: K, bC: Constraint[B])(f: A => B) extends Constraint[A] {
  override def apply(a: A) =
    bC(f(a)).leftMap(_.map(_.at(a, key))).map(_ => a)

  override def prettyPrint: String = s"at($key -> ${bC.prettyPrint})"
}

case class Switch[A](default: Constraint[A])(fc: PartialFunction[A, Constraint[A]]) extends Constraint[A] {
  override def apply(a: A): ValidationNel[ConstraintViolation[A], A] =
    fc.applyOrElse(a, (_: A) => default)(a)

  override def prettyPrint: String = s"switch()"
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

  def bySize[T](c: Constraint[Int]) = Constraint.at('size, c) following ((_:List[T]).size)

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


    def byType[A](tpe: Identifier)(implicit ty: Typer[A]) =
      Constraint.at('type, In(tpe)) following ((a: A) => {
        ty exactTypeOf a flatMap (ta =>
          flatHierarchy.getOrElse(ta, Set(ta)))
      })


    def allValuesFromConstraint(propRes: BodyStmt): Option[Constraint[List[BodyStmt]]] =
      propRes match {
        case BodyStmt.Assignment(Assignment(`owl_allValuesFrom`, ValueExp.Identifier(tpe))) =>
          Constraint.forEvery(
            Constraint.switch[BodyStmt](Constraint.success) {
              case b : BodyStmt.InstanceExp =>
                Constraint.at('instance, byType[InstanceExp](tpe)) following (_ => b.instanceExp)
              case a : BodyStmt.Assignment =>
                Constraint.at('value, Constraint.switch[ValueExp](Constraint.fail) {
                  case i : ValueExp.Identifier =>
                    Constraint.at('identifier, byType[Identifier](tpe)) following (_ => i.identifier)
                  case l : ValueExp.Literal =>
                    Constraint.at('literal, byType[Literal](tpe)) following (_ => l.literal)
                }
                ) following (_ => a.assignment.value)
            }).some
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
            allValuesFromConstraint(r)
        }
      )

    def restrictionInstance(i: InstanceExp): Option[Constraint[List[BodyStmt]]] = i match {
      case InstanceExp(propId, ConstructorApp(TpeConstructor1(`owl_propertyRestriction`, _), restrs)) =>
        (Constraint.at(propId, restrictions(restrs)) following ((_: List[BodyStmt]) collect {
          case b@BodyStmt.Assignment(Assignment(p, _)) if p == propId => b
          case b@BodyStmt.InstanceExp(InstanceExp(p, _)) if p == propId => b
        })).some
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
          (Constraint.at('properties, rs) following ((_:InstanceExp).cstrApp.body.to[List]) onlyIf byType[InstanceExp](clsId)).some
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

    val allContext =
      Constraint.at('instances, Constraint.forEvery(Constraint.applyAll(flatClasses.to[List]))) following ((_: SBEvaluatedFile).tops.to[List] collect {
        case TopLevel.InstanceExp(i) =>
          i
      })

    override def apply(a: SBEvaluatedFile) =
      allContext apply a

    override def prettyPrint: String = allContext.prettyPrint
  }

  override def fromContext(ctxt: EvalContext): ContextConstraints = new ContextConstraints(ctxt)
}

