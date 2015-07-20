package uk.co.turingatemyhamster.shortbol

import fastparse.Parser
import fastparse.core.Result
import fastparse._
import fastparse.parsers.Terminals.{Start, End}
import uk.co.turingatemyhamster.shortbol
import utest._

/**
 * Created by chris on 17/07/15.
 */
object ParserTestSuite extends TestSuite{
  val parser = new ShortbolParser()

  def shouldParse[T](txt: String, p: Parser[T], value: T): Unit = {
    (Start ~ p ~ End log "shouldParse").parse(txt) match {
      case s : Result.Success.Mutable[T] =>
        assert(s.value == value)
    }
  }

  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
    (Start ~ p ~ End log "shouldNotParse").parse(txt) match {
      case s : Result.Success.Mutable[T] =>
        assert(false)

      case f: Result.Failure.Mutable =>
        assert(true)
    }
  }

  /*scala test for automated testing - random leters ext */

  val tests = TestSuite {

    'LocalName - {
      shouldParse("anAlphaIdentifier", parser.LocalName, LocalName("anAlphaIdentifier"))
      shouldParse("a1234", parser.LocalName, LocalName("a1234"))
      shouldParse("_ajfh13", parser.LocalName,LocalName("_ajfh13"))
      shouldParse("a1234.abc-c",parser.LocalName,LocalName("a1234.abc-c"))
      shouldNotParse("1abc",parser.LocalName)
      shouldNotParse(".abc1",parser.LocalName)
      shouldNotParse("-abc1",parser.LocalName)
    }

    'NSPrefix - {
      shouldParse("anAlphaIdentifier", parser.NSPrefix, NSPrefix("anAlphaIdentifier"))
      shouldParse("a1234", parser.NSPrefix, NSPrefix("a1234"))
      shouldParse("_ajfh13", parser.NSPrefix,NSPrefix("_ajfh13"))
      shouldParse("a1234.abc-c",parser.NSPrefix,NSPrefix("a1234.abc-c"))
      shouldNotParse("1abc",parser.NSPrefix)
      shouldNotParse(".abc1",parser.NSPrefix)
      shouldNotParse("-abc1",parser.NSPrefix)
    }

    'QName - {

      shouldParse("a123:b234",parser.QName,QName(NSPrefix("a123"),LocalName("b234")))
      shouldParse("_a123.2:b234-5",parser.QName,QName(NSPrefix("_a123.2"),LocalName("b234-5")))
      shouldNotParse("._a123.2:1b234-5",parser.QName)
      shouldNotParse("abc : cba ",parser.QName)
      shouldNotParse("abc :cba ",parser.QName)
      shouldNotParse("abc: cba ",parser.QName)

    }
    'Url - {

      shouldParse("a123_.-~",parser.Url,Url("a123_.-~"))
      shouldParse("http://www.scala-lang.org/documentation/getting-started.html",parser.Url,Url("http://www.scala-lang.org/documentation/getting-started.html"))
      shouldNotParse("<www.google.co.uk>",parser.Url)
    }

    'StringLiteral - {
      shouldParse("\"I am a string with some special chars ~#¢∞^&*()£@!.\"",parser.StringLiteral,StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
      shouldNotParse("\"I am a \"string\" with some special chars ~#¢∞^&*()£@!.\"",parser.StringLiteral)
      shouldNotParse("I am not a string",parser.StringLiteral)
      shouldNotParse("\"I am half a string",parser.StringLiteral)

    }

    'IntegerLiteral - {

      shouldParse("123456789",parser.IntegerLiteral,IntegerLiteral(123456789))
//      shouldParse("-2",parser.IntegerLiteral,IntegerLiteral(-2)) /*??? cant see where negative integers are needed.*/
//      shouldNotParse("abc123",parser.IntegerLiteral,IntegerLiteral(abc123))

    }

    'Assignment - {

      shouldParse("sequence = \"aactaggactaatg\"",parser.Assignment,Assignment(LocalName("sequence"),StringLiteral("aactaggactaatg")))
      shouldParse("sequence    =        \"aactaggactaatg\"",parser.Assignment,Assignment(LocalName("sequence"),StringLiteral("aactaggactaatg")))
      shouldParse("encoding = <SBOL:DNA>",parser.Assignment,Assignment(LocalName("encoding"),QName(NSPrefix("SBOL"),LocalName("DNA"))))
      shouldParse("type = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>",parser.Assignment,Assignment(LocalName("type"),Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
      shouldParse("<SBOL:DNA> = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", parser.Assignment,Assignment(QName(NSPrefix("SBOL"),LocalName("DNA")),Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
      shouldParse("start =   22",parser.Assignment,Assignment(LocalName("start"),IntegerLiteral(22)))
      shouldParse("component = DNAComponent",parser.Assignment,Assignment(LocalName("component"),LocalName("DNAComponent")))
      shouldNotParse("\"string\" = something",parser.Assignment)
      shouldNotParse("22 = something",parser.Assignment)
      shouldNotParse("sequence =",parser.Assignment)
      shouldNotParse("sequence",parser.Assignment)
    }

    'Comment - {

      shouldParse("#this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~",parser.Comment,Comment("this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~"))
      shouldNotParse("this is a comment",parser.Comment)
      shouldParse("#",parser.Comment,Comment(""))

      /*These were initially failing as it was silently succeeding in parsing just the input before
      * the new line characters. I added 'End' at the end of the Comment parser to ensure that the entire line needs to be parsed in order to have a comment.
      * This may not be what you require, so remove End if you need to.
      * */
      shouldNotParse("#\n\r\r\ncomments can not span over multiple lines",parser.Comment)
      shouldNotParse("#I should not be \n comment",parser.Comment)
    }

    'Import - {

      shouldParse("import template_libary",parser.Import,Import("template_libary"))
      shouldParse("import /var/foo/libary/template_libary",parser.Import,Import("/var/foo/libary/template_libary"))
      shouldNotParse("importtemplate_libary",parser.Import)
      shouldNotParse("import template\n_libary",parser.Import)
    }

    'NestedInstance - {

      shouldParse("cds : DNAComponent",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("cds"),ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil
              ),Nil
            )
          )
        )
      )
      shouldParse("cds:DNAComponent",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("cds"),ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil
              ),Nil
            )
          )
        )
      )
      shouldParse(
        """cds : DNAComponent
          |
        """.stripMargin,parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil
              ),Seq(shortbol.BlankLine)
            )
          )
        )
      )
//      shouldParse(
//        """cds : DNAComponent
//          |
//          |
//        """.stripMargin,parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"),Nil
//              ),Seq(shortbol.BlankLine,shortbol.BlankLine)
//            )
//          )
//        )
//      )
//
//      /**Bug with blnakline added to end **/
//      shouldParse(
//        """cds : DNAComponent
//          |  role = <SBOL:CDS>
//        """.stripMargin,parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"),Nil
//              ),Seq(shortbol.Assignment(
//                LocalName("role"),QName(
//                  NSPrefix("SBOL"),LocalName("CDS")
//                )
//              ))
//            )
//          )
//        )
//      )
//      shouldParse(
//        """cds : DNAComponent
//          |  type = DNA
//          |  role = <SBOL:CDS>
//        """.stripMargin,parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"),Nil
//              ),Seq(shortbol.Assignment(
//                LocalName("type"),LocalName("DNA")
//                ),shortbol.Assignment(
//                LocalName("role"),QName(
//                  NSPrefix("SBOL"),LocalName("CDS")
//                )
//              ))
//            )
//          )
//        )
//      )

      shouldParse(
        "dna_sequence : DNASequence(x)",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(LocalName("x"))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(StringLiteral("AAAAGTAAAACA"))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(StringLiteral("AAAAGTAAAACA"))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "i : Inline(20,50)",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("i"), ConstructorApp(
              TpeConstructor1(
                LocalName("Inline"),Seq(IntegerLiteral(20),IntegerLiteral(50))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "pass_qname : Test(<SBOL:DNA>)",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("pass_qname"), ConstructorApp(
              TpeConstructor1(
                LocalName("Test"),Seq(QName(NSPrefix("SBOL"),LocalName("DNA")))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "pass_url : Test(<www.google.co.uk>)",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("pass_url"), ConstructorApp(
              TpeConstructor1(
                LocalName("Test"),Seq(Url("www.google.co.uk"))
              ),Nil
            )
          )
        )
      )

      shouldParse(
        "url : <www.google.co.uk>",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("url"), ConstructorApp(
              TpeConstructor1(
                Url("www.google.co.uk"),Nil)
              ,Nil)
          )
        )
      )

      shouldParse(
        "<SBOL:google> : <www.google.co.uk>",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            QName(NSPrefix("SBOL"),LocalName("google")), ConstructorApp(
              TpeConstructor1(
                Url("www.google.co.uk"),Nil)
              ,Nil)
          )
        )
      )

      shouldParse(
        "DNA : DNAComponent()",parser.NestedInstance,
        NestedInstance(
          InstanceExp(
            LocalName("DNA"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil)
              ,Nil)
          )
        )
      )


    }

    'InfixConstructorApp - {

      shouldParse(
        "a drives b", parser.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("drives"),Seq(LocalName("a"),LocalName("b"))
          ),Nil
        )
      )

      shouldNotParse(
        "adrivesb", parser.InfixConstructorApp)

      shouldNotParse(
        "1 is_not 2", parser.InfixConstructorApp)

      shouldNotParse(
      "\"a\" drives \"b\"",parser.InfixConstructorApp)

      shouldParse(
      "<SBOL:google> maps_to <www.google.co.uk>",parser.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("maps_to"),Seq(QName(NSPrefix("SBOL"),LocalName("google")),Url("www.google.co.uk"))

          ),Nil
        )
      )

    }

    'ConstructorDef - {

      shouldParse(
        "DNAComponent => ComponentDefinition",parser.ConstructorDef,
          ConstructorDef(LocalName("DNAComponent"),Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("ComponentDefinition"),Nil

              ),Nil
            )

          )
      )
      shouldParse(
        "DNAComponent=>ComponentDefinition",parser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"),Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"),Nil

            ),Nil
          )

        )
      )

      shouldParse(
        "DNASequence(x) => Sequence",parser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"),Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"),Nil

            ),Nil
          )

        )
      )

      shouldParse(
        "DNASequence  (x)  =>  Sequence",parser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"),Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"),Nil

            ),Nil
          )

        )
      )

      shouldParse("a => b(x)",parser.ConstructorDef,
        ConstructorDef(LocalName("a"),Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("b"),Seq(LocalName("x"))

            ),Nil
          )

        )
      )


    }



  }


}
