package uk.co.turingatemyhamster
package shortbol

import fastparse.all._
import CharPredicates._

object ShortbolParsers {

  implicit class ParserDecorator[T](val _p: Parser[T]) {
    def noCut = NoCut(_p)
  }

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

  lazy val QuotedStringLiteral = P(DQuote ~! (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote) map
    (s => shortbol.StringLiteral(s))

  lazy val CurlyStringLiteral = P(LBrace ~! (!RBrace ~ !Nl ~ AnyChar).rep.! ~ RBrace) map
    (s => shortbol.StringLiteral(s, escaped = true))

  lazy val StringLiteral = P(QuotedStringLiteral | CurlyStringLiteral)

  lazy val MultiLineLiteralStart = P(LBrace ~! Space.rep ~ Nl)
  lazy val MultiLineLiteralLine = P(!MultiLineLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  lazy val MultiLineLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  lazy val MultiLineLiteral = P(MultiLineLiteralStart ~! MultiLineLiteralLine.rep ~ MultiLineLiteralEnd) map
    { case (ss, i) => shortbol.MultiLineLiteral(ss map (_ substring i), i) }

  lazy val DigitSign = P(Plus | Hyphen)
  lazy val IntegerLiteral = P((DigitSign.? ~ Digit.rep(1)).!).map(_.toInt).map(shortbol.IntegerLiteral)
  lazy val ValueExp = P(Identifier | StringLiteral.noCut | MultiLineLiteral | IntegerLiteral)

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
    { case(a, r, b) => shortbol.ConstructorApp(shortbol.TpeConstructor1(r, Seq(a, b)), Seq()) }

  lazy val Comment = P(Hash ~! (!Nl ~ AnyChar).rep.!) map
    shortbol.Comment.apply

  lazy val BlankLine = P(Space.rep) map
    (_ => shortbol.BlankLine)
}

/**
 * Created by nmrp3 on 14/06/15.
 */
sealed class ShortbolParser(indent: Int) {
  import ShortbolParsers._

  lazy val IndentSpaces = P( (" " * indent) ) //log s"indent spaces $indent" /* spaces at exactly the number of indents */
  lazy val Indent = P( IndentSpaces ~ " ".rep(1).! ) map (_.length) //log s"indent starting from $indent" /* indent can be a new line repeated one or more times and if that succeeds then the current indent with at least one more space */

  def IndentBlock[T](p: ShortbolParser => Parser[T]) = Indent.flatMap { extra =>
    //println(s"got extra indent $extra")
    p(new ShortbolParser(indent + extra)) //log s"block indented by $extra"
  } //log s"indent block $indent"

  lazy val InstanceBody = P(BodyStmt.rep(sep = Nl ~ IndentSpaces)) //log "instance body"

  lazy val IndentedInstanceBody = P((SpNl ~ IndentBlock(_.InstanceBody)) | NoBody) //log "indented instance body"

  lazy val NestedInstance = InstanceExp.map(shortbol.NestedInstance)

  lazy val PrefixConstructorApp = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody) map
    (shortbol.ConstructorApp.apply _ tupled)

  lazy val ConstructorApp = P(InfixConstructorApp | PrefixConstructorApp)

  lazy val TpeConstructorStar = P(Star) map
    (_ => shortbol.TpeConstructorStar)
  lazy val TpeConstructor1 = P(Identifier ~ Space.rep ~ ValueListO) map
    (shortbol.TpeConstructor1.apply _ tupled)
  lazy val TpeConstructor = P(TpeConstructor1 | TpeConstructorStar)

  lazy val InstanceExp: Parser[shortbol.InstanceExp] = P(Identifier ~ Space.rep ~ Colon ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.InstanceExp.apply _ tupled)

  lazy val BodyStmt: Parser[shortbol.BodyStmt] = P(Assignment | NestedInstance | ConstructorApp | Comment | ShortbolParsers.BlankLine) //log "body statement"
}

object ShortbolParser extends ShortbolParser(0) {
  import ShortbolParsers._

  lazy val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.ConstructorDef.apply _ tupled)

  lazy val Import = P("import" ~ Space.rep(1) ~ Identifier) map
    shortbol.Import.apply

  lazy val TopLevel: Parser[TopLevel] = P(
    (InstanceExp | ConstructorDef | Comment | Import | Assignment | ShortbolParsers.BlankLine))

  lazy val TopLevels = P(TopLevel.rep(sep = SpNl))

  lazy val SBFile = P(Start ~ TopLevels ~ End) map
    shortbol.SBFile.apply
}

object DSL {

  import fastparse.core.Result.{Failure, Success}
  implicit class SBCHelper(val _sc: StringContext) extends AnyVal {
    protected def parse[T](p: Parser[T], args: Any*): T = p.parse(_sc.s(args :_*)) match {
      case Success(s, _) =>
        s
    }

    def short_c(args: Any*): shortbol.ConstructorDef = parse(ShortbolParser.ConstructorDef, args :_*)
    def short_a(args: Any*): shortbol.Assignment = parse(ShortbolParsers.Assignment, args :_*)
  }

}