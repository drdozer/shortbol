package uk.co.turingatemyhamster
package shortbol

import fastparse.CharPredicates._
import fastparse._

/**
 * Created by nmrp3 on 14/06/15.
 */
class ShortbolParser(indent: Int = 0, /*offset: Int = 0,*/ previousIndent : Int = 0) {

//  println(indent)
//  println(previousIndent)
//

  /**
   * Wraps another parser, succeeding/failing identically
   * but consuming no input
   */
//  case class LookaheadValue[T](p: fastparse.Parser[T]) extends fastparse.Parser[T]{
//    def parseRec(cfg: core.ParseCtx, index: Int) = {
//      p.parseRec(cfg, index) match{
//        case s: fastparse.Result.Success.Mutable[T] =>
//          success(cfg.success, s.value, index, false)
//        case f: fastparse.Result.Failure.Mutable =>
//          failMore(f, index, cfg.trace)
//      }
//    }
//    override def toString = s"&($p)"
//  }
//

//  def IndentSpaces = P( " ".rep(indent) )  /* spaces at the number of indents or more */
//  def Indent = P( "\n".rep(1) ~ IndentSpaces ) /* indent can be a new line repeated one or more times and if that succeeds then indented spaces */
//  def IndentBlock[T](p: ShortbolParser => Parser[T]) = P( LookaheadValue("\n".rep(1) ~ IndentSpaces.!) ~ Index ).flatMap{ /*Index consumes no input and provides current index of parse in the input string*/
//    case (nextIndent, offsetIndex) =>
//      if (nextIndent.length <= indent) {
//        fastparse.Fail
//      }
//      else {
//        p(new ShortbolParser(nextIndent.length, offsetIndex))
//      }
//  }



//  var currentIndent = indent
//  var prevIndent = previousIndent

//  if(currentIndent < prevIndent){
//    prevIndent = currentIndent
//  }

  var chosenIndent = indent - previousIndent

//  var howdeep = P(BlankLine.!.map(_ => indent) | ((" ".rep(indent + 1)).!.map(_.length)))
//
//  if(chosenIndent > 0){
//    howdeep = P(BlankLine.!.map(_ => indent) | ((" " * (indent + chosenIndent)).!.map(_.length)))
//  }

  var howdeep = P(((" ".rep(indent + 1)).!.map(_.length)))/* | BlankLine.!.map(_ => indent))*/


  if(chosenIndent > 0){
    howdeep = P(((" " * (indent + chosenIndent)).!.map(_.length)))/*| BlankLine.!.map(_ => indent))*/
  }


  def Deeper = howdeep //log "Deeper"

//  def BlockBody[Seq[T]] = SpNl.rep(1) ~ Deeper.flatMap(i =>
//    if(true){
//      new ShortbolParser(indent = i,indent).BodyStmts(i)
//    }
//    else{
//      new ShortbolParser(indent = i,indent).BodyStmts(i)
//    }
//  )
//
////  def test(i: Int) : Parser[Seq[shortbol.BodyStmt]] = BodyStmt(i) | BlankLine.map(_ => Seq(shortbol.BlankLine))
//
//  def BodyStmt : Parser[shortbol.BodyStmt] = P( BlankLine ~ SpNl| Assignment ~ SpNl | NestedInstance ~ SpNl | ConstructorApp ~ SpNl| Comment ~ SpNl)
//  def BodyStmts(i: Int): Parser[Seq[shortbol.BodyStmt]] =  BodyStmt.rep(1, (" " * i) ~!)


  //  def BodyStmt(i: Int): Parser[Seq[shortbol.BodyStmt]] =  P(Assignment | NestedInstance | ConstructorApp| Comment | BlankLine).rep(1, SpNl ~ (" " * i) ~!)


    def BlockBody[Seq[T]] = SpNl.rep(1) ~ Deeper.flatMap(i =>
        new ShortbolParser(indent = i,indent).BodyStmt(i)
    )

    def BodyStmt(i: Int): Parser[Seq[shortbol.BodyStmt]] =  P(Assignment | NestedInstance | ConstructorApp | Comment | BlankLine).rep(1, SpNl.rep(1) ~ (" " * i) ~!) //log "BodyStmt"



  def Colon = P(":")
  def LeftCurl = P("{")
  def RightCurl = P("}")
  def Underscore = P("_")
  def Period = P(".")
  def Hyphen = P("-")
  def Tilde = P("~")
  def Lt = P("<")
  def Gt = P(">")
  def Bang = P("!")
  def Star = P("*")
  def SQuote = P("'")
  def SQuote_¬ = P(CharPred(_ != '\''))
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
  def BackSlash = P("/")
  def Space = P(" ")
  def DDot = P("..")
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

  def QuotedIdentifier = P( Lt ~! (QName | Url) ~ Gt )
  def Identifier = P( QuotedIdentifier | LocalName )

  def DQuotedString = P(DQuote ~! (!Nl ~ DQuote_¬).rep.! ~ DQuote)
  def SQuotedString = P(SQuote ~! (!Nl ~ SQuote_¬).rep.! ~ SQuote)

  def CurlyString = P(LeftCurl ~! (!LeftCurl ~ !RightCurl ~ AnyChar).rep.! ~ RightCurl).map(_.replace("\n","").replace("\r",""))
  def StringLiteral = P(DQuotedString | SQuotedString | CurlyString).map(shortbol.StringLiteral)
  def IntegerLiteral = P(Digit.rep(1).!).map(_.toInt).map(shortbol.IntegerLiteral)
  def ValueExp = P(Identifier | StringLiteral | IntegerLiteral)

  def Assignment = P(Identifier ~ Space.rep ~ Eq ~! Space.rep ~ ValueExp).map(shortbol.Assignment.apply _ tupled) //log "Assignment"

//  def InstanceBody = P((Indent ~ BodyStmt).rep)

  def NoBody = Pass map (_ => Seq())
//  def IndentedInstanceBody = P(IndentBlock(_.InstanceBody) | NoBody)
  def IndentedInstanceBody = P(BlockBody | NoBody)
  def NestedInstance = InstanceExp.map(shortbol.NestedInstance)

  def ComaSep = P(Space.rep ~ (Coma | DDot) ~ Space.rep)
  def NoArgs = P(Pass).map(_ => Nil)
  def ArgList = P(LEllipse ~ LocalName.rep(0, ComaSep) ~ REllipse)
  def ArgListO = P(ArgList | NoArgs)

  def ValueList = P(LEllipse ~ ValueExp.rep(0, ComaSep) ~ REllipse)
  def ValueListO = P(ValueList | NoArgs)

  def InfixConstructorApp = P(Identifier ~ Space.rep ~ Identifier ~ Space.rep ~ Identifier) map
    { case(a, r, b) => shortbol.ConstructorApp(shortbol.TpeConstructor1(r, Seq(a, b)), Seq()) } //log "InflixConstructor"

  def PrefixConstructorApp = P(TpeConstructor ~ Space.rep ~ IndentedInstanceBody) map
    (shortbol.ConstructorApp.apply _ tupled) //log "PrefixConstrcutor"

  def ConstructorApp = P(InfixConstructorApp | PrefixConstructorApp) //log "ConstructorApp"

  def TpeConstructorStar = P(Star) map
    (_ => shortbol.TpeConstructorStar)
  def TpeConstructor1 = P(Identifier ~ Space.rep ~ ValueListO) map
    (shortbol.TpeConstructor1.apply _ tupled)
  def TpeConstructor = P(TpeConstructor1 | TpeConstructorStar)

  def InstanceExp: Parser[shortbol.InstanceExp] = P(Identifier ~ Space.rep ~ Colon ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.InstanceExp.apply _ tupled) //log "InstanceExpression"

//  def BodyStmt: Parser[shortbol.BodyStmt] = P(Assignment | NestedInstance | ConstructorApp | Comment | BlankLine)

  def ConstructorDef = P(Identifier ~ Space.rep ~ ArgListO ~ Space.rep ~ RightArr ~! Space.rep ~ PrefixConstructorApp) map
    (shortbol.ConstructorDef.apply _ tupled) //log "ConstructorDefinition"

  def Comment = P(Hash ~! (!Nl ~ AnyChar).rep.!) map
    shortbol.Comment.apply //log "Comment"

  def Import = P("import" ~ Space.rep(1) ~ (!Space ~ !Nl ~ AnyChar).rep(1).!) map
    shortbol.Import.apply //log "import"

//  def BlankLine = P(Start ~ Space.rep ~ End) map
//    (_ => shortbol.BlankLine) log "bl"
  def BlankLine = P(Space.rep) map
    (_ => shortbol.BlankLine) //log "BlankLine"

  def TopLevel: Parser[TopLevel] = P(
    (BlankLine ~ SpNl) |
    (InstanceExp ~ SpNl) |
      (ConstructorDef ~ SpNl) |
      (Comment ~ SpNl) |
      (Import ~ SpNl) |
      (Assignment ~ SpNl) )


  def TopLevels = P(TopLevel.rep(0," " * indent)) //log "tls"

  def File = P(Start ~ TopLevels ~ End) //log //"file"

}
