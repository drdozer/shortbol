package uk.co.turingatemyhamster.shortbol
package pragma

import ast._
import sugar._
import ops._
import ops.Eval._

import scalaz.Scalaz._
import scalaz._

object DefaultPrefixPragma {

  def apply: Hook = new Hook {
    override def register(p: Pragma) = for {
      _ <- withPHooks(pHook)
      _ <- withCHooks(cHook)
      _ <- withIHooks(iHook)
    } yield List(p)

    def pHook(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("defaultPrefix"), args) =>
        args match {
          case Seq(ValueExp.Identifier(pfx)) =>
            for {
              _ <- log(LogMessage.info(s"Registering default prefix $pfx"))
            } yield List(p)
          case _ =>
            for {
              _ <- log(LogMessage.error(s"Malformed @defaultPrefix declaration"))
            } yield Nil
        }
      case _ =>
        Eval.constant(List(p))
    }

    def iHook(i: InstanceExp): EvalState[List[InstanceExp]] =
      i.id match {
        case ln : LocalName =>
          for {
          dns <- gets((_: EvalContext).prgms.get(LocalName("defaultPrefix")).flatMap(_ headOption))
          is <- dns match {
            case Some(ps) =>
              ps.values.head match {
                case ValueExp.Identifier(LocalName(pfx)) =>
                  for {
                    _ <- log(LogMessage.info(s"Applying prefix $pfx to $ln"))
                  } yield {
                    val lnr = ln.region
                    val nsp = NSPrefix(pfx)
                    nsp.region = lnr.copy(endsAt = lnr.startsAt)
                    val qn = QName(nsp, ln)
                    qn.region = lnr
                    val ii = i.copy(id = qn)
                    ii.region = i.region
                    List(ii)
                  }
                case _ =>
                  for {
                    _ <- log(LogMessage.error(s"Malformed @defaultPrefix declaration"))
                  } yield List(i)
              }
            case None =>
              for {
                _ <- log(LogMessage.warning(
                  s"Can't resolve local name ${i.id} because no @defaultPrefix declaration is in scope"))
              } yield List(i)
          }
        } yield is
        case _ =>
          Eval.constant(List(i))
      }

    def cHook(c: ConstructorDef): EvalState[List[ConstructorDef]] =
      c.id match {
        case ln : LocalName =>
          for {
          dns <- gets((_: EvalContext).prgms.get(LocalName("defaultPrefix")).flatMap(_ headOption))
          is <- dns match {
            case Some(ps) =>
              ps.values.head match {
                case ValueExp.Identifier(LocalName(pfx)) =>
                  for {
                    _ <- log(LogMessage.info(s"Applying prefix $pfx to $ln"))
                  } yield {
                    val lnr = ln.region
                    val nsp = NSPrefix(pfx)
                    nsp.region = lnr.copy(endsAt = lnr.startsAt)
                    val qn = QName(nsp, ln)
                    qn.region = lnr
                    val cc = c.copy(id = qn)
                    cc.region = c.region
                    List(cc)
                  }
                case _ =>
                  for {
                    _ <- log(LogMessage.error(s"Malformed @defaultPrefix declaration"))
                  } yield List(c)
              }
            case None =>
              for {
                _ <- log(LogMessage.warning(
                  s"Can't resolve local name ${c.id} because no @defaultPrefix declaration is in scope"))
              } yield List(c)
          }
        } yield is
        case _ =>
          Eval.constant(List(c))
      }

    override val ID: LocalName = "defaultPrefix"

    override val bootstrap: String = "@pragma defaultPrefix pfx"
  }
}
