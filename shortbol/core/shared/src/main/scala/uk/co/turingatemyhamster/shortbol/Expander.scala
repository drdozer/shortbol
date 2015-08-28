package uk.co.turingatemyhamster.shortbol

import simulacrum.typeclass
import uk.co.turingatemyhamster.shortbol.Expander.ExState
import scalaz._
import Scalaz._

object Ops {

  def constructors(tops: List[TopLevel]): Constructors =
    Constructors(tops.collect { case cd : ConstructorDef  => cd.id -> cd } toMap)  // Creates map of constuctors

  def individuals(tops: List[TopLevel]): Individuals =
    Individuals(tops.collect { case i : InstanceExp => i.id -> i } toMap)  // Creates map of instance expressions

  def constructorsForIndividuals(cstrs: Constructors, inds: Individuals) =
    for {
      (_, i) <- inds.byId  //Here i is an Instance expression.
      TpeConstructor1(id, _) <- i.cstrApp.cstr::Nil //Getting type constructor from instance expression
      c <- cstrs.byId.get(id)  //
    } yield (i, c)

}

case class Constructors(byId: Map[Identifier, ConstructorDef])

object Constructors {
  val empty = Constructors(Map.empty)
}

case class Individuals(byId: Map[Identifier, InstanceExp])

case class Bindings(bs: Map[Identifier, ValueExp])

object Bindings {
  val empty = Bindings(Map.empty)
}

case class ExpansionContext(cstrs: Constructors, bndgs: Bindings) {

  def withConstructor(c: ConstructorDef) =
    copy(cstrs = cstrs.copy(byId = cstrs.byId + (c.id -> c)))

  def withContext(names: Seq[Identifier], values: Seq[ValueExp]) =
    copy(bndgs = bndgs.copy(bs = bndgs.bs ++ (names zip values)))

}

object ExpansionContext {
  val empty = ExpansionContext(Constructors.empty, Bindings.empty)
}

@typeclass
trait Expander[T] {
  def expansion(t: T): ExState[T]
}


object Expander {

  type ExState[T] = State[ExpansionContext, List[T]]

  import ops._

  def singleton[T](t: T): ExState[T] = List(t).point[({type l[a] = State[ExpansionContext, a]})#l]

  def noopExpander[T]: Expander[T] = new Expander[T] {
    override def expansion(t: T): ExState[T] =
      singleton(t)
  }

  def swallowExpander[T]: Expander[T] = new Expander[T] {
    override def expansion(t: T): ExState[T] =
      List.empty.point[({type l[a] = State[ExpansionContext, a]})#l]
  }

  implicit val BlankLineExpander: Expander[BlankLine.type] = swallowExpander[BlankLine.type]
  implicit val ImportExpander: Expander[Import] = swallowExpander[Import]
  implicit val CommentExpander: Expander[Comment] = swallowExpander[Comment]

  implicit val ConstructorDefExpander: Expander[ConstructorDef] = new Expander[ConstructorDef] {
    override def expansion(t: ConstructorDef) = for {
      _ <- iModify((_: ExpansionContext) withConstructor t)
    } yield Nil
  }

  implicit val AssignmentExpander: Expander[Assignment] = new Expander[Assignment] {
    override def expansion(t: Assignment): ExState[Assignment] = for {
      ps <- t.property.expansion
      vs <- t.value.expansion
      as = for {
        p <- ps
        v <- vs
      } yield Assignment(p, v)
      _ <- modify((_: ExpansionContext).withContext(ps, vs))
    } yield as
  }

  implicit val SBFileExpander: Expander[SBFile] = new Expander[SBFile] {
    override def expansion(t: SBFile): ExState[SBFile] = for {
      topss <- t.tops.to[List].traverseS(_.expansion)
    } yield SBFile(topss.flatten) :: Nil
  }

  /**
    * Returns list of top levels
    * Implicit - So I assume that scala figures out which expansion to call.
    */
  implicit val TopLevelExpander: Expander[TopLevel] = new Expander[TopLevel] {
    override def expansion(t: TopLevel): ExState[TopLevel] = t match {
      case BlankLine =>
        for { e <- BlankLine.expansion } yield e
      case i : Import =>
        for { e <- i.expansion } yield e
      case a : Assignment =>
        for { e <- a.expansion } yield e
      case c : Comment =>
        for { e <- c.expansion } yield e
      case i : InstanceExp =>
        for { e <- i.expansion } yield e
      case c : ConstructorDef =>
        for { e <- c.expansion } yield e
    }
  }

  implicit val ConstructorAppExpander: Expander[ConstructorApp] = new Expander[ConstructorApp] {
    override def expansion(t: ConstructorApp): ExState[ConstructorApp] = t.cstr match {
      case TpeConstructor1(ident, args) =>
        expandIfNeeded(t, ident, args)
    }

    def expandIfNeeded(t: ConstructorApp, ident: Identifier, args: Seq[ValueExp]): ExState[ConstructorApp] = for {
      co <- gets ((_: ExpansionContext).cstrs.byId.get(ident))
      e <- co match {
        case Some(c) =>
          expandDefinitely(c, args)
        case None =>
          singleton(t)
      }
    } yield e

    def expandDefinitely(c: ConstructorDef, args: Seq[ValueExp]): ExState[ConstructorApp] =
      for {
        bdy <- withStack(c.args, args)(c.cstrApp.body.to[List].traverseS(b => (b.expansion)))
      } yield c.cstrApp.cstr match {
        case TpeConstructor1(ident, _) => ConstructorApp(TpeConstructor1(ident, Seq()), bdy.flatten) :: Nil
      }

    def withStack[T](names: Seq[LocalName], values: Seq[ValueExp])(sf: ExState[T]): ExState[T] = for {
      ec <- get[ExpansionContext]
      _ = println(s"before: $ec")
      _ = println(s"  names: $names")
      _ = println(s" values: $values")
      _ <- modify ((_: ExpansionContext).withContext(names, values))
      ec2 <- get[ExpansionContext]
      _ = println(s"after: $ec")
      v <- sf
      _ <- put(ec)
    } yield v
  }

  implicit val InstanceExpExpander: Expander[InstanceExp] = new Expander[InstanceExp] {
    def expansion(i: InstanceExp) = {
      println(s"expansioning ${i}")
      for {
        ce <- i.cstrApp.expansion
      } yield for { c <- ce } yield InstanceExp(i.id, c)
    }
  }

  implicit val BodyStmtExpander: Expander[BodyStmt] = new Expander[BodyStmt] {
    def expansion(stmt: BodyStmt): ExState[BodyStmt] = stmt match {
      case Assignment(prop, value) =>
        for {
          pe <- prop.expansion
          ve <- value.expansion
        } yield for { p <- pe; v <- ve } yield Assignment(p, v)
      case c : ConstructorApp =>
        for { e <- c.expansion } yield e
      case NestedInstance(nested) =>
        for {
          ie <- nested.expansion
        } yield for { i <- ie } yield NestedInstance(i)
      case BlankLine =>
        for { e <- BlankLine.expansion } yield e
      case c : Comment =>
        for { e <- c.expansion } yield e
    }
  }

  implicit val IdentifierExpander: Expander[Identifier] = new Expander[Identifier] {
    def expansion(id: Identifier) = id match {
      case ln: LocalName =>
        for {
          ec <- get
        } yield (ec.bndgs.bs.get(ln).collect{ case i : Identifier => i } getOrElse id) :: Nil
      case _ =>
        singleton(id)
    }
  }

  implicit val ValueExpExpander: Expander[ValueExp] = new Expander[ValueExp] {
    def expansion(ve: ValueExp): ExState[ValueExp] = ve match {
      case ln: LocalName =>
        for {
          ec <- get
        } yield ec.bndgs.bs.getOrElse(ln, ve) :: Nil
      case _ =>
        singleton(ve)
    }
  }

}
