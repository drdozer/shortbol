package uk.co.turingatemyhamster.shortbol.ops

import shapeless._

/**
  * Created by nmrp3 on 24/06/16.
  */
trait FindAll[T] {
  object collect extends Poly1 {
    implicit val caseU = at[T](_ :: Nil)
    implicit def caseV[V] = at[V](_ => List.empty[T])
  }

  object concat extends Poly2 {
    implicit val caseAll = at[List[T], List[T]](_ ++ _)
  }

  def apply[U](u: U) = everything(collect)(concat)(u)
}

object FindAll {
  def apply[T] = new FindAll[T] {}
}