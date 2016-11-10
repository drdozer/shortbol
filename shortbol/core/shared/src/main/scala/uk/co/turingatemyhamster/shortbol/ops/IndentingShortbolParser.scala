package uk.co.turingatemyhamster.shortbol
package ops

import fastparse.all._
import fastparse.core.{Parsed, Parser}
import uk.co.turingatemyhamster.shortbol.shorthandAst.{AstNode, Pos, Region}


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

  lazy val LocalName = P(NCName.! map shorthandAst.LocalName.apply)

  lazy val NSPrefix = P( NCName.! map shorthandAst.NSPrefix.apply)

  lazy val QName = P(NSPrefix ~ Colon ~ LocalName map (shorthandAst.QName.apply _ tupled))

  lazy val UrlUnreserved = P( NCNameChar | Tilde )
  lazy val UrlReserved = P( Bang | Star | SQuote | LEllipse | REllipse | SemiColon | Colon | At | Amph | Eq | Plus | Dollar |
    Coma | Question | Percent | Hash | LBox | RBox | BackSlash)

  lazy val Url = P( Lt ~ (UrlUnreserved | UrlReserved).rep.! ~ Gt map shorthandAst.Url)

  lazy val Identifier: Parser[shorthandAst.Identifier] = P( Url | QName | LocalName)

  lazy val quotedString = DQuote ~/ (!DQuote ~ !Nl ~ AnyChar).rep.! ~ DQuote
  lazy val QuotedStringLiteral = P(quotedString map
    (s => shorthandAst.StringLiteral.SingleLine(s)))

  lazy val CurlyStringLiteral = P(LBrace ~ (!RBrace ~ !Nl ~ AnyChar).rep.! ~ RBrace map
    (s => shorthandAst.StringLiteral.SingleLine(s, escaped = true)))

  lazy val StringLiteral = P( (QuotedStringLiteral | CurlyStringLiteral | MultiLineLiteral) ~ Datatype.? ~ Language.? map
    (shorthandAst.StringLiteral.apply _ tupled))

  lazy val Datatype = P( "^^" ~ Identifier map shorthandAst.Datatype)

  lazy val LangCode = Letter.rep(1).rep(1, sep = "-")

  lazy val Language = P( "@" ~ LangCode.! map shorthandAst.Language)

  lazy val MultiLineLiteralStart = P(LBrace ~/ Space.rep ~ Nl)
  lazy val MultiLineLiteralLine = P(!MultiLineLiteralEnd ~ ((!Nl ~ AnyChar).rep ~ Nl).!)
  lazy val MultiLineLiteralEnd = P(Space.rep.! ~ RBrace) map
    (_.length)
  lazy val MultiLineLiteral = P(MultiLineLiteralStart ~/ MultiLineLiteralLine.rep ~ MultiLineLiteralEnd map
    { case (ss, i) =>
      shorthandAst.StringLiteral.MultiLine(ss.to[List] map { s => if(s.length < i) s else  s substring i}, i) })

  lazy val DigitSign = P(Plus | Hyphen)
  lazy val IntegerLiteral = P((DigitSign.? ~ Digit.rep(1)).!.map(_.toInt).map(shorthandAst.IntegerLiteral))
  lazy val Literal: Parser[shorthandAst.Literal] = StringLiteral.noCut | IntegerLiteral

  lazy val Assignment: Parser[shorthandAst.Assignment] = P(Identifier ~ Space.rep ~ Eq ~ Space.rep ~ ValueExp map
    (shorthandAst.Assignment.apply _ tupled))

  object valueExp {
    val Identifier = P(ShortbolParsers.Identifier map shorthandAst.ValueExp.Identifier)
    val Literal = P(ShortbolParsers.Literal map shorthandAst.ValueExp.Literal)
  }

  lazy val ValueExp: Parser[shorthandAst.ValueExp] =
    valueExp.Identifier | valueExp.Literal


  lazy val NoBody = Pass map (_ => List())

  lazy val ComaSep = P(Space.rep ~ Coma ~ Space.rep)
  lazy val NoArgs = P(Pass).map(_ => Nil)
  lazy val ArgList = P(LEllipse ~ Identifier.rep(0, ComaSep) ~ REllipse)
  lazy val ArgListO = P(ArgList | NoArgs)

  lazy val ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  lazy val ValueListO = P(ValueList | NoArgs)

  lazy val InfixConstructorApp: Parser[shorthandAst.ConstructorApp] = P(ValueExp ~ Space.rep ~ Identifier ~ Space.rep ~ ValueExp map
    { case(a, r, b) => shorthandAst.ConstructorApp(shorthandAst.TpeConstructor1(r, List(a, b)), List()) }) map
    (ca => {
      ca.cstr.region = ca.region
      ca})

  lazy val InfixAssignment: Parser[shorthandAst.InstanceExp] = P(Identifier ~ Space.rep ~ Eq ~ Space.rep ~ InfixConstructorApp map
    (shorthandAst.InstanceExp.apply _ tupled))

  lazy val Comment: Parser[shorthandAst.Comment] = P(Hash ~/ (!Nl ~ AnyChar).rep.! map
    shorthandAst.Comment.apply)

  lazy val BlankLine: Parser[shorthandAst.BlankLine] = P(Space.rep map
    (_ => shorthandAst.BlankLine()))

  lazy val Pragma: Parser[shorthandAst.Pragma] = P(At ~/ Identifier ~ Space.rep ~ ValueExp.rep(0, Space.rep) map
    { case (id, ves) => shorthandAst.Pragma(id, ves.to[List]) })

  lazy val TpeConstructorStar: Parser[shorthandAst.TpeConstructorStar] = P(Star map
    (_ => shorthandAst.TpeConstructorStar()))
  lazy val TpeConstructor1: Parser[shorthandAst.TpeConstructor1] = P(Identifier ~ Space.rep ~ ValueListO map
    { case (id, args) => shorthandAst.TpeConstructor1(id, args.to[List]) })
  lazy val TpeConstructor: Parser[shorthandAst.TpeConstructor] = TpeConstructor1 | TpeConstructorStar
}

/**
 * Created by nmrp3 on 14/06/15.
 */
sealed class IndentingShortbolParser(indent: Int) {
  self =>

  import ShortbolParsers._

  object bodyStmt {
    val Assignment = P(ShortbolParsers.Assignment map { a =>
      shorthandAst.PropertyExp(
        a.property,
        a.value match {
          case shorthandAst.ValueExp.Literal(lit) =>
            shorthandAst.PropertyValue.Literal(lit)
          case shorthandAst.ValueExp.Identifier(ref) =>
            shorthandAst.PropertyValue.Reference(ref)
        }
      )
    }) map shorthandAst.BodyStmt.PropertyExp
    val BlankLine = P(ShortbolParsers.BlankLine map shorthandAst.BodyStmt.BlankLine)
    val Comment = P(ShortbolParsers.Comment map shorthandAst.BodyStmt.Comment)
    val InstanceExp = P(self.InstanceExp map { ie =>
      shorthandAst.PropertyExp(
        ie.identifier,
        shorthandAst.PropertyValue.Nested(ie.cstrApp))
    }) map shorthandAst.BodyStmt.PropertyExp
    val InfixAssignment = P(ShortbolParsers.InfixAssignment map { ie =>
      shorthandAst.PropertyExp(
        ie.identifier,
        shorthandAst.PropertyValue.Nested(ie.cstrApp))
    }) map shorthandAst.BodyStmt.PropertyExp
  }

  lazy val BodyStmt: Parser[shorthandAst.BodyStmt] =
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

  lazy val InstanceBody: Parser[List[shorthandAst.BodyStmt]] = P(BodyStmt.rep(sep = Nl ~ IndentSpaces).map(_.to[List])) //log "instance body"

  lazy val IndentedInstanceBody: Parser[List[shorthandAst.BodyStmt]] = P((SpNl ~ IndentBlock(_.InstanceBody)) | NoBody) //log "indented instance body"

  lazy val PrefixConstructorApp: Parser[shorthandAst.ConstructorApp] = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody map
    (shorthandAst.ConstructorApp.apply(_ : shorthandAst.TpeConstructor, _: List[shorthandAst.BodyStmt])).tupled)

  lazy val InstanceExp: Parser[shorthandAst.InstanceExp] =
    P(Identifier ~ Space.rep ~ Colon ~/ Space.rep ~ PrefixConstructorApp map
      (shorthandAst.InstanceExp.apply _ tupled))

  lazy val ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~/ Space.rep ~ PrefixConstructorApp map
      { case (id, args, cstrApp) => shorthandAst.ConstructorDef(id, args.to[List], cstrApp) })
}

object ShortbolParser extends IndentingShortbolParser(0) {
  self =>

  import ShortbolParsers._

  object topLevel {
    val Assignment = P(ShortbolParsers.Assignment map shorthandAst.TopLevel.Assignment)
    val BlankLine = P(ShortbolParsers.BlankLine map shorthandAst.TopLevel.BlankLine)
    val Comment = P(ShortbolParsers.Comment map shorthandAst.TopLevel.Comment)
    val ConstructorDef = P(self.ConstructorDef map shorthandAst.TopLevel.ConstructorDef)
    val InstanceExp = P(self.InstanceExp map shorthandAst.TopLevel.InstanceExp)
    val Pragma = P(ShortbolParsers.Pragma map shorthandAst.TopLevel.Pragma)
  }

  lazy val TopLevel: Parser[shorthandAst.TopLevel] =
    topLevel.Pragma |
      topLevel.Comment |
      topLevel.InstanceExp |
      topLevel.ConstructorDef |
      topLevel.Assignment |
      topLevel.BlankLine

  lazy val TopLevels: Parser[List[shorthandAst.TopLevel]] = P(TopLevel.rep(sep = SpNl).map(_.to[List]))

  lazy val SBFile: Parser[shorthandAst.SBFile] = P(Start ~ TopLevels ~ End map
    (tops => shorthandAst.SBFile(tops = tops)))

  def positionAdder[T](in: shorthandAst.Identifier, txt: String, p: Parser[T]) = {
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
    def withPositions(in: shorthandAst.Identifier, txt: String) =
      positionAdder(in, txt, _p)
  }
}