package uk.co.turingatemyhamster.shortbol.longhandAst

import uk.co.turingatemyhamster.shortbol.shorthandAst

/**
 * Created by nmrp3 on 07/12/16.
 */
object Conversions {

  def toShorthand(sf: SBFile): shorthandAst.SBFile =
    shorthandAst.SBFile(sf.tops map toShorthand)

  def toShorthand(ie: InstanceExp): shorthandAst.TopLevel =
    shorthandAst.TopLevel.InstanceExp(shorthandAst.InstanceExp(
      ie.identifier,
      toShorthand(ie.cstrApp)))

  def toShorthand(ca: ConstructorApp): shorthandAst.ConstructorApp =
    shorthandAst.ConstructorApp(
      toShorthand(ca.cstr),
      ca.body map (pe => shorthandAst.BodyStmt.PropertyExp(toShorthand(pe))))

  def toShorthand(tc: TpeConstructor): shorthandAst.TpeConstructor =
    shorthandAst.TpeConstructor1(tc.tpe, Nil)

  def toShorthand(pe: PropertyExp): shorthandAst.PropertyExp =
    shorthandAst.PropertyExp(pe.property, toShorthand(pe.value))

  def toShorthand(pv: PropertyValue): shorthandAst.PropertyValue = pv match {
    case PropertyValue.Literal(lit) => shorthandAst.PropertyValue.Literal(lit)
    case PropertyValue.Reference(ref) => shorthandAst.PropertyValue.Reference(ref)
    case PropertyValue.Nested(nest) => shorthandAst.PropertyValue.Nested(toShorthand(nest))
  }

}
