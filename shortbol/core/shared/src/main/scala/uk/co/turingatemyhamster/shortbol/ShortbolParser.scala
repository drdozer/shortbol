package uk.co.turingatemyhamster
package shortbol

import fastparse._
import fastparse.CharPredicates._

object ShortbolParser {

  lazy val Colon = P(":")
  lazy val Underscore = P("_")
  lazy val Period = P(".")
  lazy val Hyphen = P("-")
  lazy val Tilde = P("~")
  lazy val Lt = P("<")
  lazy val Gt = P(">")
  lazy val Bang = P("!")
  lazy val Star = P("*")
  lazy val SQuote = P("'")
  lazy val DQuote = P("\"")
  lazy val DQuote_¬ = P(CharPred(_ != '"'))
  lazy val LEllipse = P("(")
  lazy val REllipse = P(")")
  lazy val SemiColon = P(";")
  lazy val At = P("@")
  lazy val Amph = P("&")
  lazy val Eq = P("=")
  lazy val Plus = P("+")
  lazy val Dollar = P("$")
  lazy val Coma = P(",")
  lazy val Question = P("?")
  lazy val Percent = P("%")
  lazy val Hash = P("#")
  lazy val LBox = P("[")
  lazy val RBox = P("]")
  lazy val LBrace = P("{")
  lazy val RBrace = P("}")
  lazy val BackSlash = P("/")
  lazy val Space = P(" ")
  lazy val Nl = P("\r\n" | "\r" | "\n")
  lazy val SpNl = P(Space.rep ~ Nl)

  lazy val RightArr = P("=>")


  lazy val Letter = P( CharPred(isLetter) )
  lazy val Digit = P( CharPred(isDigit) )

  lazy val NCName0 = P( Letter | Underscore )
  lazy val NCNameChar = P( NCName0 | Digit | Period | Hyphen )
  lazy val NCName = P( NCName0 ~ NCNameChar.rep )

  lazy val LocalName = P( NCName.! )
    .map(shortbol.LocalName.apply)
  lazy val NSPrefix = P( NCName.! )
    .map(shortbol.NSPrefix.apply)
  lazy val QName = P(NSPrefix ~ Colon ~ LocalName)
    .map(shortbol.QName.apply _ tupled)

  lazy val UrlUnreserved = P( NCNameChar | Tilde )
  lazy val UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar |
    Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  lazy val Url = P( (UrlUnreserved | UrlReserved).rep.! ).map(shortbol.Url)

  lazy val QuotedIdentifier = P(Lt ~! (QName | Url) ~ Gt)
  lazy val Identifier = P( QuotedIdentifier | LocalName )

  lazy val ShortStringLiteral = P(DQuote ~! (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote) map
    (s => shortbol.StringLiteral(s))

  lazy val MultiLineStringLiteralStart = P(LBrace ~ Space.rep ~ Nl)
  lazy val MultiLineStringLiteralLine = P(!MultiLineStringLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  lazy val MultiLineStringLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  lazy val MultiLineStringLiteral = P(MultiLineStringLiteralStart ~! MultiLineStringLiteralLine.rep ~ MultiLineStringLiteralEnd) map
    { case (s, i) => shortbol.StringLiteral(s map (_ substring i) mkString, multiLine = true) }

  lazy val StringLiteral = P(ShortStringLiteral | MultiLineStringLiteral)
  lazy val IntegerLiteral = P(Digit.rep(1).!).map(_.toInt).map(shortbol.IntegerLiteral)
  lazy val ValueExp = P(Identifier | StringLiteral | IntegerLiteral)

  lazy val Assignment = P(Identifier ~ Space.rep ~ Eq ~! Space.rep ~ ValueExp) map
    (shortbol.Assignment.apply _ tupled)

  lazy val NoBody = Pass map (_ => Seq())

  lazy val ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  lazy val NoArgs = P(Pass).map(_ => Nil)
  lazy val ArgList = P(LEllipse ~ LocalName.rep(0, ComaSep) ~ REllipse)
  lazy val ArgListO = P(ArgList | NoArgs)

  lazy val ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  lazy val ValueListO = P(ValueList | NoArgs)

  lazy val InfixConstructorApp = P(Identifier ~ Space.rep ~ Identifier ~ Space.rep ~ Identifier) map
    { case(a, r, b) => shortbol.ConstructorApp(shortbol.TpeConstructor1(r, Seq(a, b)), Seq()) } log "cstrPfx"

  lazy val Comment = P(Hash ~! (!Nl ~ AnyChar).rep.!) map
    shortbol.Comment.apply log "cmt"

  lazy val Import = P("import" ~ Space.rep(1) ~ (!Space ~ !Nl ~ AnyChar).rep(1).!) map
    shortbol.Import.apply log "import"

  lazy val BlankLine = P(Space.rep) map
    (_ => shortbol.BlankLine) log "bl"

  lazy val parser = new ShortbolParser
}

/**
 * Created by nmrp3 on 14/06/15.
 */
class ShortbolParser(indent: Int = 0, offset: Int = 0) {
  import ShortbolParser._

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

  lazy val IndentSpaces = P( " ".rep(indent) )  /* spaces at the number of indents or more */
  lazy val Indent = P( "\n".rep(1) ~ IndentSpaces ) /* indent can be a new line repeated one or more times and if that succeeds then indented spaces */
  def IndentBlock[T](p: ShortbolParser => Parser[T]) = P( LookaheadValue("\n".rep(1) ~ IndentSpaces.!) ~ Index ).flatMap{ /*Index consumes no input and provides current index of parse in the input string*/
    case (nextIndent, offsetIndex) =>
      if (nextIndent.length <= indent) {
        fastparse.Fail
      }
      else {
        p(new ShortbolParser(nextIndent.length, offsetIndex))
      }
  }

  lazy val InstanceBody = P((Indent ~ BodyStmt).rep)

  lazy val IndentedInstanceBody = P(IndentBlock(_.InstanceBody) | NoBody)

  lazy val NestedInstance = InstanceExp.map(shortbol.NestedInstance)

  lazy val PrefixConstructorApp = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody) map
    (shortbol.ConstructorApp.apply _ tupled) log "cstrPfx"

  lazy val ConstructorApp = P(InfixConstructorApp | PrefixConstructorApp) log "cstrApp"

  lazy val TpeConstructorStar = P(Star) map
    (_ => shortbol.TpeConstructorStar)
  lazy val TpeConstructor1 = P(Identifier ~ Space.rep ~ ValueListO) map
    (shortbol.TpeConstructor1.apply _ tupled)
  lazy val TpeConstructor = P(TpeConstructor1 | TpeConstructorStar)

  lazy val InstanceExp: Parser[shortbol.InstanceExp] = P(Identifier ~ Space.rep ~ Colon ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.InstanceExp.apply _ tupled) log "inst"

  lazy val BodyStmt: Parser[shortbol.BodyStmt] = P(Assignment | NestedInstance | ConstructorApp | Comment | ShortbolParser.BlankLine)

  lazy val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.ConstructorDef.apply _ tupled) log "cstr"

  lazy val TopLevel: Parser[TopLevel] = P(
    (InstanceExp | ConstructorDef | Comment | Import | ShortbolParser.BlankLine | Assignment ~ SpNl)) log "tl"

  lazy val TopLevels = P(TopLevel.rep(sep = SpNl)) log "tls"

  lazy val File = P(Start ~ TopLevels ~ End) log "file"
}
