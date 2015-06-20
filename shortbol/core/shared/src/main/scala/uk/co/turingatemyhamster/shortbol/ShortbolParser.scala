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

  def IndentSpaces = P( " ".rep(indent) )
  def Indent = P( "\n".rep(1) ~ IndentSpaces )
  def IndentBlock[T](p: ShortbolParser => Parser[T]) = P( LookaheadValue("\n".rep(1) ~ IndentSpaces.!) ~ Index ).flatMap{
    case (nextIndent, offsetIndex) =>
      if (nextIndent.length <= indent) {
        fastparse.Fail
      }
      else {
        p(new ShortbolParser(nextIndent.length, offsetIndex))
      }
  }

  def Colon = P(":")
  def Underscore = P("_")
  def Period = P(".")
  def Hyphen = P("-")
  def Tilde = P("~")
  def Lt = P("<")
  def Gt = P(">")
  def Bang = P("!")
  def Star = P("*")
  def SQuote = P("'")
  def DQuote = P("\"")
  def DQuote_¬ = P(CharPred(_ != '"'))
  def LEllipse = P("(")
  def REllipse = P(")")
  def SemiColon = P(";")
  def At = P("@")
  def Amph = P("&")
  def Eq = P("=")
  def Plus = P("+")
  def Dollar = P("$")
  def Coma = P(",")
  def Question = P("?")
  def Percent = P("%")
  def Hash = P("#")
  def LBox = P("[")
  def RBox = P("]")
  def LBrace = P("{")
  def RBrace = P("}")
  def BackSlash = P("/")
  def Space = P(" ")
  def Nl = P("\r\n" | "\r" | "\n")
  def SpNl = P(Space.rep ~ Nl)

  def RightArr = P("=>")


  def Letter = P( CharPred(isLetter) )
  def Digit = P( CharPred(isDigit) )

  def NCName0 = P( Letter | Underscore )
  def NCNameChar = P( NCName0 | Digit | Period | Hyphen )
  def NCName = P( NCName0 ~ NCNameChar.rep )

  def LocalName = P( NCName.! )
    .map(shortbol.LocalName.apply)
  def NSPrefix = P( NCName.! )
    .map(shortbol.NSPrefix.apply)
  def QName = P(NSPrefix ~ Colon ~ LocalName)
    .map(shortbol.QName.apply _ tupled)

  def UrlUnreserved = P( NCNameChar | Tilde )
  def UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar |
    Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  def Url = P( (UrlUnreserved | UrlReserved).rep.! ).map(shortbol.Url)

  def QuotedIdentifier = P(Lt ~! (QName | Url) ~ Gt)
  def Identifier = P( QuotedIdentifier | LocalName )

  def ShortStringLiteral = P(DQuote ~! (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote) map
    (s => shortbol.StringLiteral(s))

  def MultiLineStringLiteralStart = P(LBrace ~ Space.rep ~ Nl)
  def MultiLineStringLiteralLine = P(!MultiLineStringLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  def MultiLineStringLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  def MultiLineStringLiteral = P(MultiLineStringLiteralStart ~! MultiLineStringLiteralLine.rep ~ MultiLineStringLiteralEnd) map
    { case (s, i) => shortbol.StringLiteral(s map (_ substring i) mkString, multiLine = true) }

  def StringLiteral = P(ShortStringLiteral | MultiLineStringLiteral)
  def IntegerLiteral = P(Digit.rep(1).!).map(_.toInt).map(shortbol.IntegerLiteral)
  def ValueExp = P(Identifier | StringLiteral | IntegerLiteral)

  def Assignment = P(Identifier ~ Space.rep ~ Eq ~! Space.rep ~ ValueExp).map(shortbol.Assignment.apply _ tupled)

  def InstanceBody = P((Indent ~ BodyStmt).rep)

  def NoBody = Pass map (_ => Seq())
  def IndentedInstanceBody = P(IndentBlock(_.InstanceBody) | NoBody)

  def NestedInstance = InstanceExp.map(shortbol.NestedInstance)

  def ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  def NoArgs = P(Pass).map(_ => Nil)
  def ArgList = P(LEllipse ~ LocalName.rep(0, ComaSep) ~ REllipse)
  def ArgListO = P(ArgList | NoArgs)

  def ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  def ValueListO = P(ValueList | NoArgs)

  def InfixConstructorApp = P(Identifier ~ Space.rep ~ Identifier ~ Space.rep ~ Identifier) map
    { case(a, r, b) => shortbol.ConstructorApp(shortbol.TpeConstructor1(r, Seq(a, b)), Seq()) } log "cstrPfx"

  def PrefixConstructorApp = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody) map
    (shortbol.ConstructorApp.apply _ tupled) log "cstrPfx"

  def ConstructorApp = P(InfixConstructorApp | PrefixConstructorApp) log "cstrApp"

  def TpeConstructorStar = P(Star) map
    (_ => shortbol.TpeConstructorStar)
  def TpeConstructor1 = P(Identifier ~ Space.rep ~ ValueListO) map
    (shortbol.TpeConstructor1.apply _ tupled)
  def TpeConstructor = P(TpeConstructor1 | TpeConstructorStar)

  def InstanceExp: Parser[shortbol.InstanceExp] = P(Identifier ~ Space.rep ~ Colon ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.InstanceExp.apply _ tupled) log "inst"

  def BodyStmt: Parser[shortbol.BodyStmt] = P(Assignment | NestedInstance | ConstructorApp | Comment | BlankLine)

  def ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.ConstructorDef.apply _ tupled) log "cstr"

  def Comment = P(Hash ~! (!Nl ~ AnyChar).rep.!) map
    shortbol.Comment.apply log "cmt"

  def Import = P("import" ~ Space.rep(1) ~ (!Space ~ !Nl ~ AnyChar).rep(1).!) map
    shortbol.Import.apply log "import"

  def BlankLine = P(Space.rep) map
    (_ => shortbol.BlankLine) log "bl"

  def TopLevel: Parser[TopLevel] = P(
    (InstanceExp | ConstructorDef | Comment | Import | BlankLine | Assignment ~ SpNl)) log "tl"

  def TopLevels = P(TopLevel.rep(sep = SpNl)) log "tls"

  def File = P(Start ~ TopLevels ~ End) log "file"
}
