package uk.co.turingatemyhamster
package shortbol

import fastparse._
import fastparse.CharPredicates._

/**
 * Created by nmrp3 on 14/06/15.
 */
class ShortbolParser(indent: Int = 0, offset: Int = 0) {

  /**
   * Wraps another parser, succeeding/failing identically
   * but consuming no input
   */
  case class LookaheadValue[T](p: fastparse.Parser[T]) extends fastparse.Parser[T]{
    def parseRec(cfg: core.ParseCtx, index: Int) = {
      p.parseRec(cfg, index) match{
        case s: fastparse.Result.Success.Mutable[T] =>
          success(cfg.success, s.value, index, false)
        case f: fastparse.Result.Failure.Mutable =>
          failMore(f, index, cfg.trace)
      }
    }
    override def toString = s"&($p)"
  }

  val IndentSpaces = P( " ".rep(indent) )
  val Indent = P( "\n".rep(1) ~ IndentSpaces )
  def IndentBlock[T](p: ShortbolParser => Parser[T]) = P( LookaheadValue("\n".rep(1) ~ IndentSpaces.!) ~ Index ).flatMap{
    case (nextIndent, offsetIndex) =>
      if (nextIndent.length <= indent) {
        fastparse.Fail
      }
      else {
        p(new ShortbolParser(nextIndent.length, offsetIndex))
      }
  }

  val Colon = P(":")
  val Underscore = P("_")
  val Period = P(".")
  val Hyphen = P("-")
  val Tilde = P("~")
  val Lt = P("<")
  val Gt = P(">")
  val Bang = P("!")
  val Star = P("*")
  val SQuote = P("'")
  val DQuote = P("\"")
  val DQuote_¬ = P(CharPred(_ != '"'))
  val LEllipse = P("(")
  val REllipse = P(")")
  val SemiColon = P(";")
  val At = P("@")
  val Amph = P("&")
  val Eq = P("=")
  val Plus = P("+")
  val Dollar = P("$")
  val Coma = P(",")
  val Question = P("?")
  val Percent = P("%")
  val Hash = P("#")
  val LBox = P("[")
  val RBox = P("]")
  val BackSlash = P("/")
  val Space = P(" ")
  val Nl = P("\n")

  val RightArr = P("=>")


  val Letter = P( CharPred(isLetter) )
  val Digit = P( CharPred(isDigit) )

  val NCName0 = P( Letter | Underscore )
  val NCNameChar = P( NCName0 | Digit | Period | Hyphen )
  val NCName = P( NCName0 ~ NCNameChar.rep )

  val LocalName = P( NCName.! )
    .map(shortbol.LocalName.apply)
  val NSPrefix = P( NCName.! )
    .map(shortbol.NSPrefix.apply)
  val QName = P(NSPrefix ~ Colon ~ LocalName)
    .map(shortbol.QName.apply _ tupled)

  val UrlUnreserved = P( NCNameChar | Tilde )
  val UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar
    | Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  val Url = P( (UrlUnreserved | UrlReserved).rep.! ).map(shortbol.Url)

  val QuotedIdentifier = P(Lt ~ (QName | Url) ~ Gt)
  val Identifier = P( QuotedIdentifier | LocalName )

  val StringLiteral = P(DQuote ~ DQuote_¬.rep.! ~ DQuote).map(shortbol.StringLiteral)
  val IntegerLiteral = P(Digit.rep(1).!).map(_.toInt).map(shortbol.IntegerLiteral)
  val ValueExp = P(Identifier | StringLiteral | IntegerLiteral)

  val Assignment = P(Identifier ~ Space.rep ~ Eq ~ Space.rep ~ ValueExp).map(shortbol.Assignment.apply _ tupled)

  val InstanceBody = P((Indent ~ BodyStmt).rep)

  val NoBody = P("\n").map(_ => Nil)
  val IndentedInstanceBody = P(IndentBlock(_.InstanceBody) | NoBody)

  val NestedInstance = InstanceExp.map(shortbol.NestedInstance)
  
  val NestedAssignment = P(Identifier ~ Space.rep
    ~ IndentedInstanceBody).map(shortbol.NestedAssignment.apply _ tupled)

  val ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  val ArgList = P(LEllipse ~ LocalName.rep(0, ComaSep) ~ REllipse)
  val ArgListO = P(ArgList | Pass.map(_ => Nil))

  val ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  val ValueListO = P(ValueList | Pass.map(_ => Nil))
  
  val TpeConstructor = P(Identifier ~ Space.rep ~ ValueListO).map(shortbol.TpeConstructor.apply _ tupled)

  lazy val InstanceExp: Parser[shortbol.InstanceExp] = P(Identifier ~ Space.rep ~ Colon ~ Space.rep ~ TpeConstructor ~ Space.rep
    ~ IndentedInstanceBody).map(shortbol.InstanceExp.apply _ tupled)

  lazy val BodyStmt: Parser[shortbol.BodyStmt] = P(Assignment | NestedInstance | NestedAssignment)

  val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~ Space.rep ~ TpeConstructor ~ Space.rep
    ~ IndentedInstanceBody).map(shortbol.ConstructorDef.apply _ tupled)


  val TopLevel: Parser[TopLevel] = P((InstanceExp ~ Nl.rep) | (ConstructorDef ~ Nl.rep))
  val TopLevels = P(TopLevel.rep)
}
