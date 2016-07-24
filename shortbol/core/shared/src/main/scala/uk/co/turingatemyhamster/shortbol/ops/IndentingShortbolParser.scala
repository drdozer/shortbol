package uk.co.turingatemyhamster.shortbol
package ops

import fastparse.all._
import fastparse.core.{Parsed, Parser}
import uk.co.turingatemyhamster.shortbol.ast.{AstNode, Pos, Region}


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

  lazy val LocalName = P(NCName.! map ast.LocalName.apply)

  lazy val NSPrefix = P( NCName.! map ast.NSPrefix.apply)

  lazy val QName = P(NSPrefix ~ Colon ~ LocalName map (ast.QName.apply _ tupled))

  lazy val UrlUnreserved = P( NCNameChar | Tilde )
  lazy val UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar |
    Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  lazy val Url = P( Lt ~/ (UrlUnreserved | UrlReserved).rep.! ~/ Gt map ast.Url)

  lazy val Identifier: Parser[ast.Identifier] = P( Url | QName | LocalName)

  lazy val quotedString = DQuote ~/ (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote
  lazy val QuotedStringLiteral = P(quotedString map
    (s => ast.StringLiteral.SingleLine(s)))

  lazy val CurlyStringLiteral = P(LBrace ~ (!RBrace ~ !Nl ~ AnyChar).rep.! ~ RBrace map
    (s => ast.StringLiteral.SingleLine(s, escaped = true)))

  lazy val StringLiteral = P( (QuotedStringLiteral | CurlyStringLiteral | MultiLineLiteral) ~ Datatype.? ~ Language.? map
    (ast.StringLiteral.apply _ tupled))

  lazy val Datatype = P( "^^" ~ Identifier map ast.Datatype)

  lazy val LangCode = Letter.rep(1).rep(1, sep = "-")

  lazy val Language = P( "@" ~ LangCode.! map ast.Language)

  lazy val MultiLineLiteralStart = P(LBrace ~/ Space.rep ~ Nl)
  lazy val MultiLineLiteralLine = P(!MultiLineLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  lazy val MultiLineLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  lazy val MultiLineLiteral = P(MultiLineLiteralStart ~/ MultiLineLiteralLine.rep ~ MultiLineLiteralEnd map
    { case (ss, i) =>
      ast.StringLiteral.MultiLine(ss map { s => if(s.length < i) s else  s substring i}, i) })

  lazy val DigitSign = P(Plus | Hyphen)
  lazy val IntegerLiteral = P((DigitSign.? ~ Digit.rep(1)).!.map(_.toInt).map(ast.IntegerLiteral))
  lazy val Literal: Parser[ast.Literal] = StringLiteral.noCut | IntegerLiteral

  lazy val Assignment: Parser[ast.Assignment] = P(Identifier ~ Space.rep ~ Eq ~ Space.rep ~ ValueExp map
    (ast.Assignment.apply _ tupled))

  object valueExp {
    val Identifier = P(ShortbolParsers.Identifier map ast.ValueExp.Identifier)
    val Literal = P(ShortbolParsers.Literal map ast.ValueExp.Literal)
  }

  lazy val ValueExp: Parser[ast.ValueExp] =
    valueExp.Identifier | valueExp.Literal


  lazy val NoBody = Pass map (_ => Seq())

  lazy val ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  lazy val NoArgs = P(Pass).map(_ => Nil)
  lazy val ArgList = P(LEllipse ~ Identifier.rep(0, ComaSep) ~ REllipse)
  lazy val ArgListO = P(ArgList | NoArgs)

  lazy val ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  lazy val ValueListO = P(ValueList | NoArgs)

  lazy val InfixConstructorApp: Parser[ast.ConstructorApp] = P(ValueExp ~ Space.rep ~ Identifier ~ Space.rep ~ ValueExp map
    { case(a, r, b) => ast.ConstructorApp(ast.TpeConstructor1(r, Seq(a, b)), Seq()) }) map
    (ca => {
      ca.cstr.region = ca.region
      ca})

  lazy val InfixAssignment: Parser[ast.InstanceExp] = P(Identifier ~ Space.rep ~ Eq ~ Space.rep ~ InfixConstructorApp map
    (ast.InstanceExp.apply _ tupled))

  lazy val Comment: Parser[ast.Comment] = P(Hash ~/ (!Nl ~ AnyChar).rep.! map
    ast.Comment.apply)

  lazy val BlankLine: Parser[ast.BlankLine] = P(Space.rep map
    (_ => ast.BlankLine()))

  lazy val Pragma: Parser[ast.Pragma] = P(At ~/ Identifier ~ Space.rep ~ ValueExp.rep(0, Space.rep) map
    (ast.Pragma.apply _ tupled))

  lazy val TpeConstructorStar: Parser[ast.TpeConstructorStar] = P(Star map
    (_ => ast.TpeConstructorStar()))
  lazy val TpeConstructor1: Parser[ast.TpeConstructor1] = P(Identifier ~ Space.rep ~ ValueListO map
    (ast.TpeConstructor1.apply _ tupled))
  lazy val TpeConstructor: Parser[ast.TpeConstructor] = TpeConstructor1 | TpeConstructorStar
}

/**
 * Created by nmrp3 on 14/06/15.
 */
sealed class IndentingShortbolParser(indent: Int) {
  self =>

  import ShortbolParsers._

  object bodyStmt {
    val Assignment = P(ShortbolParsers.Assignment map ast.BodyStmt.Assignment)
    val BlankLine = P(ShortbolParsers.BlankLine map ast.BodyStmt.BlankLine)
    val Comment = P(ShortbolParsers.Comment map ast.BodyStmt.Comment)
    val InstanceExp = P(self.InstanceExp map ast.BodyStmt.InstanceExp)
    val InfixAssignment = P(ShortbolParsers.InfixAssignment map ast.BodyStmt.InstanceExp)
  }

  lazy val BodyStmt: Parser[ast.BodyStmt] =
    bodyStmt.InfixAssignment |
      bodyStmt.Assignment |
      bodyStmt.InstanceExp |
      bodyStmt.Comment |
      bodyStmt.BlankLine //log "body statement"

  lazy val IndentSpaces = P(" " * indent) //log s"indent spaces $indent" /* spaces at exactly the number of indents */
  lazy val Indent = P( IndentSpaces ~ " ".rep(1).! ) map (_.length) //log s"indent starting from $indent" /* indent can be a new line repeated one or more times and if that succeeds then the current indent with at least one more space */

  def IndentBlock[T](p: IndentingShortbolParser => Parser[T]) = Indent.flatMap { extra =>
    //println(s"got extra indent $extra")
    p(new IndentingShortbolParser(indent + extra)) //log s"block indented by $extra"
  } //log s"indent block $indent"

  lazy val InstanceBody: Parser[Seq[ast.BodyStmt]] = P(BodyStmt.rep(sep = Nl ~ IndentSpaces)) //log "instance body"

  lazy val IndentedInstanceBody: Parser[Seq[ast.BodyStmt]] = P((SpNl ~ IndentBlock(_.InstanceBody)) | NoBody) //log "indented instance body"

  lazy val PrefixConstructorApp: Parser[ast.ConstructorApp] = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody map
    (ast.ConstructorApp.apply _ tupled))

  lazy val InstanceExp: Parser[ast.InstanceExp] =
    P(Identifier ~ Space.rep ~ Colon ~/ Space.rep ~ PrefixConstructorApp map
      (ast.InstanceExp.apply _ tupled))

  lazy val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~/ Space.rep ~ PrefixConstructorApp map
      (ast.ConstructorDef.apply _ tupled))
}

object ShortbolParser extends IndentingShortbolParser(0) {
  self =>

  import ShortbolParsers._

  object topLevel {
    val Assignment = P(ShortbolParsers.Assignment map ast.TopLevel.Assignment)
    val BlankLine = P(ShortbolParsers.BlankLine map ast.TopLevel.BlankLine)
    val Comment = P(ShortbolParsers.Comment map ast.TopLevel.Comment)
    val ConstructorDef = P(self.ConstructorDef map ast.TopLevel.ConstructorDef)
    val InstanceExp = P(self.InstanceExp map ast.TopLevel.InstanceExp)
    val Pragma = P(ShortbolParsers.Pragma map ast.TopLevel.Pragma)
  }

  lazy val TopLevel: Parser[ast.TopLevel] =
    topLevel.Pragma |
      topLevel.Comment |
      topLevel.InstanceExp |
      topLevel.ConstructorDef |
      topLevel.Assignment |
      topLevel.BlankLine

  lazy val TopLevels: Parser[Seq[ast.TopLevel]] = P(TopLevel.rep(sep = SpNl))

  lazy val SBFile: Parser[ast.SBFile] = P(Start ~ TopLevels ~ End map
    (tops => ast.SBFile(tops = tops)))

  def positionAdder[T](in: ast.Identifier, txt: String, p: Parser[T]) = {
    lazy val indexes = -1::buildTable(txt)
    lazy val table = indexes.zip(1 to indexes.length).reverse

    def buildTable(txt: String, from: Int = 0): List[Int] =
      txt.indexOf('\n', from) match {
        case -1 => Nil
        case i => i :: buildTable(txt, i+1)
      }

    def rc(i: Int) = {
      val (p, row) = table.dropWhile(_._1 > i).head
      (row, i-p)
    }

    def instrument(rule: Parser[_], from: Int, result: () => Parsed[_]): Unit = result() match {
      case s : Parsed.Success[_] =>
        s.value match {
          case a : AstNode =>
            val to = s.index
            val (fr, fc) = rc(from)
            val (tr, tc) = rc(to)
            a.region=Region(startsAt = Pos(from, fr, fc), endsAt = Pos(to, tr, tc), in)
          case _ =>
        }
      case _ =>
    }

    p.parse(txt, instrument = instrument)
  }

  implicit class POps[T](val _p: Parser[T]) extends AnyVal {
    def withPositions(in: ast.Identifier, txt: String) =
      positionAdder(in, txt, _p)
  }
}