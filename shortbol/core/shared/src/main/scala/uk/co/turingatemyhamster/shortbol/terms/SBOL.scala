package uk.co.turingatemyhamster.shortbol
package terms

import sharedAst.{sugar, NSPrefix}
import sugar._

/**
  * Created by nmrp3 on 23/11/16.
  */
object SBOL {
  val sbol : NSPrefix = "sbol"

  val displayId           = sbol :# "displayId"
  val interaction         = sbol :# "interaction"
  val participation       = sbol :# "participation"
  val participant         = sbol :# "participant"
  val functionalComponent = sbol :# "functionalComponent"
  val FunctionalComponent = sbol :# "FunctionalComponent"
  val access              = sbol :# "access"
  val access_public       = sbol :# "public"
  val definition          = sbol :# "definition"
  val direction           = sbol :# "direction"
  val inout               = sbol :# "inout"
  val ModuleDefinition    = sbol :# "ModuleDefinition"
  val elements            = sbol :# "elements"
  val Sequence            = sbol :# "Sequence"
  val sequence            = sbol :# "sequence"
  val component           = sbol :# "component"
  val sequenceConstraint  = sbol :# "sequenceConstraint"
  val subject             = sbol :# "subject"
  val `object`            = sbol :# "object"
  val sequenceAnnotation  = sbol :# "sequenceAnnotation"
  val ComponentDefinition = sbol :# "ComponentDefinition"
  val Component           = sbol :# "Component"
  val SequenceConstraint  = sbol :# "SequenceConstraint"
  val module              = sbol :# "module"
  val Module              = sbol :# "Module"
  val MapsTo              = sbol :# "MapsTo"
  val mapsTo              = sbol :# "mapsTo"
  val local               = sbol :# "local"
  val remote              = sbol :# "remote"

}
