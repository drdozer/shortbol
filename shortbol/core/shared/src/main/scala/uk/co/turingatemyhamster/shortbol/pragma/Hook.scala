package uk.co.turingatemyhamster.shortbol
package pragma

import ast.Pragma
import ops.Eval.EvalState

/**
  * Created by nmrp3 on 22/06/16.
  */
trait Hook {
  /**
    * Register hook call-backs for this registered pragma.
    *
    * @param p the meta-pragma that registers the pragma
    * @return unit run for side-effects
    */
  def register(p: Pragma): EvalState[List[Pragma]]
}
