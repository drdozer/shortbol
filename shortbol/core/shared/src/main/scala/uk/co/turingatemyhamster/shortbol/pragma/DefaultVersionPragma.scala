package uk.co.turingatemyhamster.shortbol.pragma

import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalState
import uk.co.turingatemyhamster.shortbol.sharedAst._
import sugar._
import uk.co.turingatemyhamster.shortbol.shorthandAst.Pragma

import scalaz.Scalaz._
import scalaz._

/**
  * Created by nmrp3 on 28/11/16.
  */
object DefaultVersionPragma {
  self =>

  def apply: Hook = new Hook {
    /**
      * Register hook call-backs for this registered pragma.
      *
      * @param p the meta-pragma that registers the pragma
      * @return unit run for side-effects
      */
    override def register(p: Pragma): EvalState[List[Pragma]] = List(p).point[EvalState]

    override def ID: LocalName = self.ID

    override def bootstrap: String = self.bootstrap
  }

  val ID: LocalName = "defaultVersion"

  val bootstrap: String = "@pragma defaultVersion version"
}
