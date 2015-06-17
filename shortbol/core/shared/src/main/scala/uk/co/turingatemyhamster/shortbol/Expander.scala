package uk.co.turingatemyhamster.shortbol

import shapeless.ops.record.Values
import simulacrum.typeclass

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

}

case class Constructors(byId: Map[Identifier, ConstructorDef])

case class Individuals(byId: Map[Identifier, InstanceExp])

case class Bindings(bs: Map[LocalName, ValueExp])

@typeclass
trait Expander[T] {
  def expandWith(t: T, ec: ExpansionContext): T
}

object Expander {

  import ops._

  implicit def SeqExpander[T](implicit st: Expander[T]): Expander[Seq[T]] = new Expander[Seq[T]] {
    override def expandWith(t: Seq[T], ec: ExpansionContext): Seq[T] = t map (st.expandWith(_, ec))
  }

  implicit val TopLevelExpander: Expander[TopLevel] = new Expander[TopLevel] {
    override def expandWith(t: TopLevel, ec: ExpansionContext): TopLevel = t match {
      case i : InstanceExp => i expandWith ec
      case _ => t
    }
  }

  implicit val InstanceExpExpander: Expander[InstanceExp] = new Expander[InstanceExp] {
    def expandWith(i: InstanceExp, ec: ExpansionContext): InstanceExp =
      ec.cstrs.byId.get(i.cstr.id) map { c =>
        InstanceExp(
          id = i.id,
          cstr = c.cstr,
          body = (c.body ++ i.body) expandWith ec.withContext(c.args, i.cstr.args))
      } getOrElse i
  }

  implicit val BodyStmtExpander: Expander[BodyStmt] = new Expander[BodyStmt] {
    def expandWith(stmt: BodyStmt, ec: ExpansionContext): BodyStmt = stmt match {
      case Assignment(prop, value) =>
        Assignment(prop expandWith ec, value expandWith ec)
      case NestedAssignment(prop, body) =>
        NestedAssignment(prop expandWith ec, body expandWith ec)
      case NestedInstance(nested) =>
        NestedInstance(nested expandWith ec)
    }
  }

  implicit val IdentifierExpander: Expander[Identifier] = new Expander[Identifier] {
    def expandWith(id: Identifier, ec: ExpansionContext): Identifier = id match {
      case ln: LocalName =>
        ec.bndgs.bs.get(ln).collect{ case i : Identifier => i } getOrElse id
      case _ => id
    }
  }

  implicit val ValueExpExpander: Expander[ValueExp] = new Expander[ValueExp] {
    def expandWith(ve: ValueExp, ec: ExpansionContext): ValueExp = ve match {
      case ln: LocalName =>
        ec.bndgs.bs.get(ln) getOrElse ve
      case _ => ve
    }
  }

}

case class ExpansionContext(cstrs: Constructors, bndgs: Bindings) {

  def withContext(names: Seq[LocalName], values: Seq[ValueExp]) =
    copy(cstrs, bndgs.copy(bndgs.bs ++ (names zip values)))

}
