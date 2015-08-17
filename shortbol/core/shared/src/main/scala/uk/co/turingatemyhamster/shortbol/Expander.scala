package uk.co.turingatemyhamster.shortbol

import simulacrum.typeclass

object Ops {

  def constructors(tops: Seq[TopLevel]): Constructors =
    Constructors(tops.collect { case cd : ConstructorDef  => cd.id -> cd } toMap)  // Creates map of constuctors

  def individuals(tops: Seq[TopLevel]): Individuals =
    Individuals(tops.collect { case i : InstanceExp => i.id -> i } toMap)  // Creates map of instance expressions

  def constructorsForIndividuals(cstrs: Constructors, inds: Individuals) =
    for {
      (_, i) <- inds.byId  //Here i is an Instance expression.
      TpeConstructor1(id, _) <- i.cstrApp.cstr::Nil //Getting type constructor from instance expression
      c <- cstrs.byId.get(id)  //
    } yield (i, c)

}

case class Constructors(byId: Map[Identifier, ConstructorDef])

case class Individuals(byId: Map[Identifier, InstanceExp])

case class Bindings(bs: Map[LocalName, ValueExp])

@typeclass
trait Expander[T] {
  def expandWith(t: T, ec: ExpansionContext): Seq[T]
}


object Expander {

  import ops._

  /** *
    * Returns Sequence of top levels
    * Implicit - So I assume that scala figures out which expandWith to call.
    */
  implicit val TopLevelExpander: Expander[TopLevel] = new Expander[TopLevel] {
    override def expandWith(t: TopLevel, ec: ExpansionContext): Seq[TopLevel] = t match {
      case i : InstanceExp =>
        i expandWith ec
      case _ =>
        Seq(t)
    }
  }

  implicit val ConstructorAppExpander: Expander[ConstructorApp] = new Expander[ConstructorApp] {
    override def expandWith(t: ConstructorApp, ec: ExpansionContext): Seq[ConstructorApp] = t.cstr match {
      case TpeConstructor1(id, args) =>
        println(s"expanding ${t} with ${ec}")
        ec.cstrs.byId.get(id) map { c =>
          Seq(
            ConstructorApp(
            c.cstrApp.cstr,
            c.cstrApp.body flatMap (_ expandWith ec.withContext(c.args, args))))
        } getOrElse Seq(t)
    }
  }

  implicit val InstanceExpExpander: Expander[InstanceExp] = new Expander[InstanceExp] {
    def expandWith(i: InstanceExp, ec: ExpansionContext): Seq[InstanceExp] = {
      println(s"expanding ${i} with ${ec}")
      for {
        c <- i.cstrApp expandWith ec
      } yield InstanceExp(i.id, c)
    }
  }

  implicit val BodyStmtExpander: Expander[BodyStmt] = new Expander[BodyStmt] {
    def expandWith(stmt: BodyStmt, ec: ExpansionContext): Seq[BodyStmt] = stmt match {
      case Assignment(prop, value) =>
        for {
          p <- prop expandWith ec
          v <- value expandWith ec
        } yield Assignment(p, v)
      case c : ConstructorApp =>
        c expandWith ec
      case NestedInstance(nested) =>
        for {
          i <- nested expandWith ec
        } yield NestedInstance(i)
    }
  }

  implicit val IdentifierExpander: Expander[Identifier] = new Expander[Identifier] {
    def expandWith(id: Identifier, ec: ExpansionContext): Seq[Identifier] = id match {
      case ln: LocalName =>
        Seq(ec.bndgs.bs.get(ln).collect{ case i : Identifier => i } getOrElse id)
      case _ =>
        Seq(id)
    }
  }

  implicit val ValueExpExpander: Expander[ValueExp] = new Expander[ValueExp] {
    def expandWith(ve: ValueExp, ec: ExpansionContext): Seq[ValueExp] = ve match {
      case ln: LocalName =>
        Seq(ec.bndgs.bs.getOrElse(ln, ve))
      case _ =>
        Seq(ve)
    }
  }

}

case class ExpansionContext(cstrs: Constructors, bndgs: Bindings) {

  def withContext(names: Seq[LocalName], values: Seq[ValueExp]) =
    copy(cstrs, bndgs.copy(bndgs.bs ++ (names zip values)))

}
