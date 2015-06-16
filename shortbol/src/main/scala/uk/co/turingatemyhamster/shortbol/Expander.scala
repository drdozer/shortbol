package uk.co.turingatemyhamster.shortbol

import shapeless.ops.record.Values

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


trait Expander[T] {
  def expand(t: T): T
}

case class ExpansionContext(cstrs: Constructors, bndgs: Bindings) {

  def expansion[T](t: T)(implicit te: Expander[T]): T = te.expand(t)

  implicit def SeqExpander[T](implicit st: Expander[T]): Expander[Seq[T]] = new Expander[Seq[T]] {
    override def expand(t: Seq[T]): Seq[T] = t map st.expand
  }

  implicit object TopLevelExpander extends Expander[TopLevel] {
    override def expand(t: TopLevel): TopLevel = t match {
      case i : InstanceExp => expansion(i)(InstanceExpExpander)
      case _ => t
    }
  }

  implicit object InstanceExpExpander extends Expander[InstanceExp] {
    def expand(i: InstanceExp): InstanceExp =
      cstrs.byId.get(i.cstr.id) map { c =>
        InstanceExp(
          id = i.id,
          cstr = c.cstr,
          body = withContext(c.args, i.cstr.args).expansion(c.body)(SeqExpander) ++ expansion(i.body))
      } getOrElse i
  }

  implicit object BodyStmtExpander extends Expander[BodyStmt] {
    def expand(stmt: BodyStmt): BodyStmt = stmt match {
      case Assignment(prop, value) =>
        Assignment(expansion(prop), expansion(value))
      case NestedAssignment(prop, body) =>
        NestedAssignment(expansion(prop), expansion(body))
      case NestedInstance(nested) =>
        NestedInstance(expansion(nested))
    }
  }

  implicit object IdentifierExpander extends Expander[Identifier] {
    def expand(id: Identifier): Identifier = id match {
      case ln: LocalName =>
        bndgs.bs.get(ln).collect{ case i : Identifier => i } getOrElse id
    }
  }

  implicit object ValueExpExpander extends Expander[ValueExp] {
    def expand(ve: ValueExp): ValueExp = ve match {
      case ln: LocalName =>
        bndgs.bs.get(ln).collect{ case i : Identifier => i } getOrElse ve
    }
  }

  def withContext(names: Seq[LocalName], values: Seq[ValueExp]) =
    copy(cstrs, bndgs.copy(bndgs.bs ++ (names zip values)))
}
