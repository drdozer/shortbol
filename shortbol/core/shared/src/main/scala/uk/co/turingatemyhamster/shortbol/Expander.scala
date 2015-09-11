package uk.co.turingatemyhamster.shortbol

import simulacrum.typeclass
import uk.co.turingatemyhamster.shortbol.Expander.ExState
import scalaz._
import Scalaz._


case class ExpansionContext(rslvr: Resolver,
                            rctxt: ResolutionContext = ResolutionContext(None, PrefixBindingsImpl(None, Map.empty)),
                            cstrs: Map[Identifier, ConstructorDef] = Map.empty,
                            bndgs: Map[Identifier, ValueExp] = Map.empty,
                            thrwn: Seq[Throwable] = Seq.empty)
{

  def withConstructor(c: ConstructorDef) =
    copy(cstrs = cstrs + (c.id -> c))

  def withContext(names: Seq[Identifier], values: Seq[ValueExp]) =
    copy(bndgs = bndgs ++ (names zip values))


}

@typeclass
trait Expander[T] {
  self =>
  def expansion(t: T): ExState[T]
  final def log(msg: String) = new Expander[T] {
    override def expansion(t: T): ExState[T] = for {
      e <- self.expansion(t)
    } yield {
        e
      }
  }
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
  implicit val CommentExpander: Expander[Comment] = swallowExpander[Comment]

  implicit val ImportExpander: Expander[Import] = new Expander[Import] {
    override def expansion(t: Import): ExState[Import] = t match {
      case UnprocessedImport(path) =>
        for {
          resolver <- gets ((_: ExpansionContext).rslvr)
          ctxt <- gets ((_: ExpansionContext).rctxt)
          i <- resolver.resolve(ctxt, path) match {
            case \/-(imported) =>
              for {
                ex <- imported.expansion
              } yield ProcessedImport(path, SBFile(ex.map(_.tops).flatten)) :: Nil
            case -\/(err) =>
              for {
                _ <- modify((ec: ExpansionContext) => ec.copy(thrwn = ec.thrwn :+ err))
              } yield t :: Nil
          }
        } yield i
    }
  }

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

  implicit def SeqExpander[T : Expander]: Expander[Seq[T]] = new Expander[Seq[T]] {
    override def expansion(ts: Seq[T]): ExState[Seq[T]] =
      for {
        es <- ts.to[List].traverseS(b => (b.expansion))
      } yield es
  }

  implicit val ConstructorAppExpander: Expander[ConstructorApp] = new Expander[ConstructorApp] {
    override def expansion(t: ConstructorApp): ExState[ConstructorApp] = t.cstr match {
      case TpeConstructor1(ident, args) =>
        for {
          es <- expandIfNeeded(t, ident, args)
          bs <- withStack(Seq(), Seq())(t.body.expansion)
        } yield for {
          e <- es
        } yield {
          e.copy(body = e.body ++ bs.flatten)
        }
    }

    def expandIfNeeded(t: ConstructorApp, ident: Identifier, args: Seq[ValueExp]): ExState[ConstructorApp] = for {
      co <- gets ((_: ExpansionContext).cstrs.get(ident))
      e <- co match {
        case Some(c) =>
          expandDefinitely(c, args)
        case None =>
          singleton(t.copy(body = Seq())) // there's no body from an unexpanded template
      }
    } yield e

    def expandDefinitely(c: ConstructorDef, args: Seq[ValueExp]): ExState[ConstructorApp] =
      for {
        bdy <- withStack(c.args, args)(c.cstrApp.body.expansion)
        cd <- (c.cstrApp.cstr match {
          case TpeConstructor1(ident, _) => ConstructorApp(TpeConstructor1(ident, Seq()), bdy.flatten)
        }).expansion
      } yield cd

    def withStack[T](names: Seq[LocalName], values: Seq[ValueExp])(sf: ExState[T]): ExState[T] = for {
      ec <- get[ExpansionContext]
      _ <- modify ((_: ExpansionContext).withContext(names, values))
      v <- sf
      _ <- put(ec) // fixme: we should be only overwriting the bindings
    } yield v
  }

  implicit val InstanceExpExpander: Expander[InstanceExp] = new Expander[InstanceExp] {
    def expansion(i: InstanceExp) = {
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
        } yield (ec.bndgs.get(ln).collect{ case i : Identifier => i } getOrElse id) :: Nil
      case _ =>
        singleton(id)
    }
  }

  implicit val ValueExpExpander: Expander[ValueExp] = new Expander[ValueExp] {
    def expansion(ve: ValueExp): ExState[ValueExp] = ve match {
      case ln: LocalName =>
        for {
          ec <- get
        } yield ec.bndgs.getOrElse(ln, ve) :: Nil
      case _ =>
        singleton(ve)
    }
  }

}
