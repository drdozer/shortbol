package uk.co.turingatemyhamster.shortbol.shorthandAst

import uk.co.turingatemyhamster.shortbol.longhandAst

/**
 * Created by nmrp3 on 07/12/16.
 */
object Conversions {

  def toLonghand(pv: PropertyValue): longhandAst.PropertyValue = pv match {
    case PropertyValue.Literal(lit) => longhandAst.PropertyValue.Literal(lit)
    case PropertyValue.Reference(ref) => longhandAst.PropertyValue.Reference(ref)
    case PropertyValue.Nested(nest) => ??? /* longhandAst.PropertyValue.Nested(toLonghand(nest)) */
  }

}
