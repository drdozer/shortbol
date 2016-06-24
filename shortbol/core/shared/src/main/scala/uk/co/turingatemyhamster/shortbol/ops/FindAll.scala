package uk.co.turingatemyhamster.shortbol.ops

import shapeless._

/**
  * Created by nmrp3 on 24/06/16.
  */
trait FindAll[T, U] {
  def apply(t: T, us: List[U]): List[U]
}

object FindAll extends FindAll_0 {
  def apply[T, U](implicit e: FindAll[T, U]) = e

  implicit def deriveCNil[U]: FindAll[CNil, U] = ???

  implicit def deriveCCons[H, T <: Coproduct, U](implicit he: FindAll[H, U], te: FindAll[T, U]): FindAll[H :+: T, U] = new FindAll[:+:[H, T], U] {
    override def apply(t: :+:[H, T], us: List[U]): List[U] = t match {
      case Inl(head) => he(head, us)
      case Inr(tail) => te(tail, us)
    }
  }

  implicit def deriveHNil[U]: FindAll[HNil, U] = new FindAll[HNil, U] {
    override def apply(t: HNil, us: List[U]): List[U] = us
  }

  implicit def deriveHCons[H, T <: HList, U](implicit he: FindAll[H, U], te: FindAll[T, U]): FindAll[H::T, U] = new FindAll[H::T, U] {
    override def apply(t: ::[H, T], us: List[U]): List[U] = te(t.tail, he(t.head, us))
  }

  implicit def deriveGeneric[T, G, U](implicit gen: Generic.Aux[T, G], ge: FindAll[G, U]): FindAll[T, U] = new FindAll[T, U] {
    override def apply(t: T, us: List[U]): List[U] = ge(gen.to(t), us)
  }
}

trait FindAll_0 {

  implicit def hit[T]: FindAll[T, T] = new FindAll[T, T] {
    override def apply(t: T, us: List[T]): List[T] = t :: us
  }

  implicit def miss[T, U]: FindAll[T, U] = new FindAll[T, U] {
    override def apply(t: T, us: List[U]): List[U] = us
  }

}