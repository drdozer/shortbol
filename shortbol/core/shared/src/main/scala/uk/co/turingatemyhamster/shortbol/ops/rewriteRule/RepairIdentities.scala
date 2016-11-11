package uk.co.turingatemyhamster.shortbol
package ops
package rewriteRule

import shorthandAst.sugar._
import longhandAst.{InstanceExp, PropertyExp}
import longhandAst.sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
object RepairIdentities {
  final private val rdf_about = "rdf" :# "about"

  import optics.longhand.InstanceExp._
  import optics.longhand.ConstructorApp._

  lazy val instanceExpRequiresAbout: RewriteRule[InstanceExp] = RewriteRule { (ie: InstanceExp) =>
    (cstrApp composeLens body) modify
      ((rdf_about := ie.identifier : PropertyExp) :: _) apply
      ie
  } at { (ie: InstanceExp) =>
    ie.cstrApp.body.collectFirst{ case PropertyExp(`rdf_about`, _) => () }.isEmpty
  }
}
