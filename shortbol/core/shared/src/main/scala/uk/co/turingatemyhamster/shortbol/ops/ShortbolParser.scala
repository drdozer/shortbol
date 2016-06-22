package uk.co.turingatemyhamster.shortbol
package ops

import fastparse.all._
import fastparse.parsers.Combinators


object ShortbolParsers {


  implicit class ParserDecorator[T](val _pt: Parser[T]) {
    def noCut = NoCut(_pt)
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


  lazy val Letter = P( CharIn('a' to 'z') | CharIn('A' to 'Z') )
  lazy val Digit = P( CharIn('0' to '9') )

  lazy val NCName0 = P( Letter | Underscore )
  lazy val NCNameChar = P( NCName0 | Digit | Period | Hyphen )
  lazy val NCName = P( NCName0 ~ NCNameChar.rep )

  lazy val LocalName = P( NCName.! )
    .map(ast.LocalName.apply)
  lazy val NSPrefix = P( NCName.! )
    .map(ast.NSPrefix.apply)
  lazy val QName = P(NSPrefix ~ Colon ~ LocalName)
    .map(ast.QName.apply _ tupled)

  lazy val UrlUnreserved = P( NCNameChar | Tilde )
  lazy val UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar |
    Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  lazy val Url = P( (UrlUnreserved | UrlReserved).rep.! ).map(ast.Url)

  lazy val QuotedIdentifier = P(Lt ~/ Url ~/ Gt)
  lazy val Identifier: Parser[ast.Identifier] = P( QuotedIdentifier | QName | LocalName)

  lazy val QuotedStringLiteral = P(DQuote ~/ (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote) map
    (s => ast.StringLiteral(s))

  lazy val CurlyStringLiteral = P(LBrace ~/ (!RBrace ~ !Nl ~ AnyChar).rep.! ~ RBrace) map
    (s => ast.StringLiteral(s, escaped = true))

  lazy val StringLiteral = P(QuotedStringLiteral | CurlyStringLiteral)

  lazy val MultiLineLiteralStart = P(LBrace ~/ Space.rep ~ Nl)
  lazy val MultiLineLiteralLine = P(!MultiLineLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  lazy val MultiLineLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  lazy val MultiLineLiteral = P(MultiLineLiteralStart ~/ MultiLineLiteralLine.rep ~ MultiLineLiteralEnd) map
    { case (ss, i) => ast.MultiLineLiteral(ss map (_ substring i), i) }

  lazy val DigitSign = P(Plus | Hyphen)
  lazy val IntegerLiteral = P((DigitSign.? ~ Digit.rep(1)).!).map(_.toInt).map(ast.IntegerLiteral)
  lazy val Literal = P(StringLiteral.noCut | MultiLineLiteral | IntegerLiteral)

  lazy val Assignment: Parser[ast.Assignment] = P(Identifier ~ Space.rep ~ Eq ~/ Space.rep ~ ValueExp) map
    (ast.Assignment.apply _ tupled)

  object valueExp {
    val Identifier = ShortbolParsers.Identifier map ast.ValueExp.Identifier
    val Literal = ShortbolParsers.Literal map ast.ValueExp.Literal
  }

  lazy val ValueExp: Parser[ast.ValueExp] = P(
    valueExp.Identifier | valueExp.Literal)


  lazy val NoBody = Pass map (_ => Seq())

  lazy val ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  lazy val NoArgs = P(Pass).map(_ => Nil)
  lazy val ArgList = P(LEllipse ~ LocalName.rep(0, ComaSep) ~ REllipse)
  lazy val ArgListO = P(ArgList | NoArgs)

  lazy val ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  lazy val ValueListO = P(ValueList | NoArgs)

  lazy val InfixConstructorApp: Parser[ast.ConstructorApp] = P(ValueExp ~ Space.rep ~ Identifier ~ Space.rep ~ ValueExp) map
    { case(a, r, b) => ast.ConstructorApp(ast.TpeConstructor1(r, Seq(a, b)), Seq()) }

  lazy val Comment: Parser[ast.Comment] = P(Hash ~/ (!Nl ~ AnyChar).rep.!) map
    ast.Comment.apply

  lazy val BlankLine: Parser[ast.BlankLine.type] = P(Space.rep) map
    (_ => ast.BlankLine)

  lazy val TpeConstructorStar: Parser[ast.TpeConstructorStar.type] = P(Star) map
    (_ => ast.TpeConstructorStar)
  lazy val TpeConstructor1: Parser[ast.TpeConstructor1] = P(Identifier ~ Space.rep ~ ValueListO) map
    (ast.TpeConstructor1.apply _ tupled)
  lazy val TpeConstructor: Parser[ast.TpeConstructor] = P(TpeConstructor1 | TpeConstructorStar)
}

/**
 * Created by nmrp3 on 14/06/15.
 */
sealed class ShortbolParser(indent: Int) {
  self =>

  import ShortbolParsers._

  object bodyStmt {
    val Assignment = ShortbolParsers.Assignment map ast.BodyStmt.Assignment
    val BlankLine = ShortbolParsers.BlankLine map ast.BodyStmt.BlankLine
    val Comment = ShortbolParsers.Comment map ast.BodyStmt.Comment
    val InstanceExp = self.InstanceExp map ast.BodyStmt.InstanceExp
    val ConstructorApp = self.ConstructorApp map ast.BodyStmt.ConstructorApp
  }

  lazy val BodyStmt: Parser[ast.BodyStmt] = P(
    bodyStmt.Assignment |
      bodyStmt.InstanceExp |
      bodyStmt.ConstructorApp |
      bodyStmt.Comment |
      bodyStmt.BlankLine) //log "body statement"

  lazy val IndentSpaces = P( (" " * indent) ) //log s"indent spaces $indent" /* spaces at exactly the number of indents */
  lazy val Indent = P( IndentSpaces ~ " ".rep(1).! ) map (_.length) //log s"indent starting from $indent" /* indent can be a new line repeated one or more times and if that succeeds then the current indent with at least one more space */

  def IndentBlock[T](p: ShortbolParser => Parser[T]) = Indent.flatMap { extra =>
    //println(s"got extra indent $extra")
    p(new ShortbolParser(indent + extra)) //log s"block indented by $extra"
  } //log s"indent block $indent"

  lazy val InstanceBody: Parser[Seq[ast.BodyStmt]] = P(BodyStmt.rep(sep = Nl ~ IndentSpaces)) //log "instance body"

  lazy val IndentedInstanceBody: Parser[Seq[ast.BodyStmt]] = P((SpNl ~ IndentBlock(_.InstanceBody)) | NoBody) //log "indented instance body"

  lazy val PrefixConstructorApp: Parser[ast.ConstructorApp] = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody) map
    (ast.ConstructorApp.apply _ tupled)

  lazy val ConstructorApp: Parser[ast.ConstructorApp] = P(InfixConstructorApp | PrefixConstructorApp)

  lazy val InstanceExp: Parser[ast.InstanceExp] =
    P(Identifier ~ Space.rep ~ Colon ~/ Space.rep ~ PrefixConstructorApp) map
      (ast.InstanceExp.apply _ tupled)

  lazy val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~/ Space.rep ~ PrefixConstructorApp) map
      (ast.ConstructorDef.apply _ tupled)
}

object ShortbolParser extends ShortbolParser(0) {
  self =>

  import ShortbolParsers._

  object topLevel {
    val Assignment = ShortbolParsers.Assignment map ast.TopLevel.Assignment
    val BlankLine = ShortbolParsers.BlankLine map ast.TopLevel.BlankLine
    val Import = P("import" ~ Space.rep(1) ~ Identifier) map ast.TopLevel.Import
    val Comment = ShortbolParsers.Comment map ast.TopLevel.Comment
    val InstanceExp = self.InstanceExp map ast.TopLevel.InstanceExp
    val ConstructorDef = self.ConstructorDef map ast.TopLevel.ConstructorDef
  }

  lazy val TopLevel = P(
    topLevel.Import |
      topLevel.Comment |
      topLevel.InstanceExp |
      topLevel.ConstructorDef |
      topLevel.Assignment |
      topLevel.BlankLine)

  lazy val TopLevels: Parser[Seq[ast.TopLevel]] = P(TopLevel.rep(sep = SpNl))

  lazy val SBFile: Parser[ast.SBFile] = P(Start ~ TopLevels ~ End) map
    (tops => ast.SBFile(tops = tops))
}

object DSL {

  import fastparse.core.Parsed.Success
  implicit class SBCHelper(val _sc: StringContext) extends AnyVal {
    protected def parse[T](p: Parser[T], args: Any*): T = p.parse(_sc.s(args :_*)) match {
      case Success(s, _) =>
        s
    }

    def short_c(args: Any*): ast.TopLevel.ConstructorDef = parse(ShortbolParser.topLevel.ConstructorDef, args :_*)
    def short_a(args: Any*): ast.Assignment = parse(ShortbolParsers.Assignment, args :_*)
  }

}