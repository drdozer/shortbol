package uk.co.turingatemyhamster.shortbol
package terms

import sharedAst.{sugar, NSPrefix}
import sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
object OWL {

  val owl : NSPrefix = "owl"

  val `class` = owl :# "class"
  val subClassOf = owl :# "subClassOf"
  val propertyRestriction = owl :# "propertyRestriction"
  val minCardinality = owl :# "minCardinality"
  val maxCardinality = owl :# "maxCardinality"
  val exactCardinality = owl :# "exactCardinality"
  val allValuesFrom = owl :# "allValuesFrom"


}
