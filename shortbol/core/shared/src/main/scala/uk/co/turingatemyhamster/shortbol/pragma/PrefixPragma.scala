package uk.co.turingatemyhamster.shortbol
package pragma

import ast._
import ast.sugar._
import ops.Eval.{EvalState, log, withCHooks, withIHooks, withPHooks}
import ops._

import scalaz.Scalaz._
import scalaz._

/**
  * Created by nmrp3 on 24/06/16.
  */
object PrefixPragma {
  self =>

  def apply: Hook = new Hook {
    var logLevel: LogLevel = LogLevel.Warning

    override def register(p: Pragma) = for {
      _ <- withPHooks(pHook)
      _ <- withIHooks(iHook)
      _ <- withCHooks(cHook)
    } yield List(p)

    def pHook(p: Pragma): EvalState[List[Pragma]] = p match {
      case Pragma(LocalName("prefix"), args) =>
        args match {
          case Seq(ValueExp.Identifier(LocalName(pfx)), ValueExp.Identifier(Url(url))) =>
            for {
              _ <- log(LogMessage.info(s"Registered prefix mapping $pfx to $url", p.region))
            } yield List(p)
          case _ =>
            for {
              _ <- log(LogMessage.error(s"Malformed @prefix pragma with $args", p.region))
            } yield Nil
        }
      case _ =>
        List(p).point[EvalState]
    }

    val instanceIDs = AllIdentifiers[InstanceExp]
    val constructorIDs = AllIdentifiers[ConstructorDef]

    def iHook(i: InstanceExp): EvalState[List[InstanceExp]] = for {
      _ <- checkIdentifiers(instanceIDs(i))
    } yield List(i)

    def cHook(c: ConstructorDef): EvalState[List[ConstructorDef]] = for {
      _ <- checkIdentifiers(constructorIDs(c))
    } yield List(c)

    def checkIdentifiers[T](is: Seq[Identifier]): EvalState[List[Unit]] =
      (is.to[List] map {
        case QName(n@NSPrefix(pfx), _) =>
          for {
            found <- gets((_: EvalContext).prgms.get(ID).to[Vector].flatten collect {
              case Pragma(_, Seq(ValueExp.Identifier(LocalName(p)), url)) if p == pfx => url
            })
            _ <- if (found.isEmpty) {
              log(LogMessage(s"No prefix binding for $pfx", logLevel, n.region, None))
            } else {
              ().point[EvalState]
            }
          } yield ()
        case _ =>
          ().point[EvalState]
      }).sequenceU

    override val ID: LocalName = self.ID

    override val bootstrap: String = self.bootstrap
  }

  val ID: LocalName = "prefix"

  val bootstrap: String = "@pragma prefix pfx url"

  def resolve(qn: QName): EvalState[Option[Url]] =
    gets((_: EvalContext).prgms.get(ID).to[Vector].flatten collect {
      case Pragma(_, Seq(ValueExp.Identifier(LocalName(p)), ValueExp.Identifier(Url(url)))) if p == qn.prefix.pfx =>
        Url(url ++ qn.localName.name)
    } headOption)
}
