package uk.co.turingatemyhamster.shortbol

/**
 * Created by nmrp3 on 15/06/15.
 */
class Expander {



}

object Ops {

  def constructors(tops: Seq[TopLevel]): Constructors =
    Constructors(tops.collect { case cd : ConstructorDef  => cd.id -> cd } toMap)

  def individuals(tops: Seq[TopLevel]): Individuals =
    Individuals(tops.collect { case i : InstanceExp => i.id -> i } toMap)

  def constructorsForIndividuals(cstrs: Constructors, inds: Individuals) =
    for {
      (_, i) <- inds.byId
      c <- cstrs.byId.get(i.cstr.id)
    } yield (i, c)

  def expand(i: InstanceExp, c: ConstructorDef) =
    InstanceExp(i.id, c.cstr, c.body)
}

case class Constructors(byId: Map[Identifier, ConstructorDef])

case class Individuals(byId: Map[Identifier, InstanceExp])