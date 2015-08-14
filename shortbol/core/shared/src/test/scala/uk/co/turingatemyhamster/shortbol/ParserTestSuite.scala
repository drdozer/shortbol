package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Result.Failure
import fastparse.core.Result.Success
import uk.co.turingatemyhamster.shortbol
import utest._

/**
 * Created by chris on 17/07/15.
 */
object ParserTestSuite extends TestSuite{
  val parser = new ShortbolParser()

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }


  def shouldParse[T](txt: String, p: Parser[T], value: T): Unit = {
    println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case s : Success[T] =>
        assert(s.value == value)
    }
  }

  def shouldParse[T](txt: String, p: Parser[T]): Unit = {
    println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case s : Success[T] =>
        assert(true)
    }

  }

  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
    println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case s : Success[T] =>
        assert(false)

      case f: Failure =>
        assert(true)
    }
  }

  /*scala test for automated testing - random leters ext */

  val tests = TestSuite {
    'LocalName - {
      'accepts - {
        * - shouldParse("anAlphaIdentifier", ShortbolParser.LocalName, LocalName("anAlphaIdentifier"))
        * - shouldParse("a1234", ShortbolParser.LocalName, LocalName("a1234"))
        * - shouldParse("_ajfh13", ShortbolParser.LocalName, LocalName("_ajfh13"))
        * - shouldParse("a1234.abc-c", ShortbolParser.LocalName, LocalName("a1234.abc-c"))
      }

      'rejects - {
        * - shouldNotParse("1abc", ShortbolParser.LocalName)
        * - shouldNotParse(".abc1", ShortbolParser.LocalName)
        * - shouldNotParse("-abc1", ShortbolParser.LocalName)
      }
    }

    'NSPrefix - {
      'accepts - {
        * - shouldParse("anAlphaIdentifier", ShortbolParser.NSPrefix, NSPrefix("anAlphaIdentifier"))
        * - shouldParse("a1234", ShortbolParser.NSPrefix, NSPrefix("a1234"))
        * - shouldParse("_ajfh13", ShortbolParser.NSPrefix, NSPrefix("_ajfh13"))
        * - shouldParse("a1234.abc-c", ShortbolParser.NSPrefix, NSPrefix("a1234.abc-c"))
      }

      'rejects - {
        * - shouldNotParse("1abc", ShortbolParser.NSPrefix)
        * - shouldNotParse(".abc1", ShortbolParser.NSPrefix)
        * - shouldNotParse("-abc1", ShortbolParser.NSPrefix)
      }
    }

    'QName - {
      'accepts - {
        * - shouldParse("a123:b234", ShortbolParser.QName, QName(NSPrefix("a123"), LocalName("b234")))
        * - shouldParse("_a123.2:b234-5", ShortbolParser.QName, QName(NSPrefix("_a123.2"), LocalName("b234-5")))
      }

      'rejects - {
        * - shouldNotParse("._a123.2:1b234-5", ShortbolParser.QName)
        * - shouldNotParse("abc : cba ", ShortbolParser.QName)
        * - shouldNotParse("abc :cba ", ShortbolParser.QName)
        * - shouldNotParse("abc: cba ", ShortbolParser.QName)
      }
    }

    'Url - {
      'accepts - {
        * - shouldParse("a123_.-~", ShortbolParser.Url, Url("a123_.-~"))
        * - shouldParse("http://www.scala-lang.org/documentation/getting-started.html", ShortbolParser.Url, Url("http://www.scala-lang.org/documentation/getting-started.html"))
      }

      'rejects - {
        * - shouldNotParse("<www.google.co.uk>", ShortbolParser.Url)
      }
    }

    'StringLiteral - {
      'using_StringLiteral - {
        'accepts - {
          shouldParse("\"I am a string with some special chars ~#¢∞^&*()£@!.\"", ShortbolParser.StringLiteral, StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
        }

        'rejects - {
          * - shouldNotParse("\"I am a \"string\" with some special chars ~#¢∞^&*()£@!.\"", ShortbolParser.StringLiteral)
          * - shouldNotParse("I am not a string", ShortbolParser.StringLiteral)
          * - shouldNotParse("\"I am half a string", ShortbolParser.StringLiteral)
          * - shouldNotParse("\"I am not\nastring\"", ShortbolParser.StringLiteral)
        }
      }

      'using_ValueExp - {
        shouldParse("\"I am a string with some special chars ~#¢∞^&*()£@!.\"", ShortbolParser.ValueExp, StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
      }
    }

    'CurlyLiteral - {
      'rejects - {
        * - shouldNotParse("{I am a multiline \nstring}", ShortbolParser.StringLiteral)
        * - shouldNotParse("{\nI am a string\n}", ShortbolParser.StringLiteral)
        * - shouldNotParse("{\nI am\na string\n}", ShortbolParser.StringLiteral)
      }

      'accepts - {
        * - shouldParse("{I am also a string}", ShortbolParser.StringLiteral, StringLiteral("I am also a string", escaped = true))
        * - shouldParse("{ I am also a string }", ShortbolParser.StringLiteral, StringLiteral(" I am also a string ", escaped = true))
      }
    }

    'MultiLineLiteral - {
      'using_MultiLineLiteral - {
        'rejects - {
          * - shouldNotParse("{I am a multiline \nstring}", ShortbolParser.MultiLineLiteral)
          * - shouldNotParse("{I am a \nmultiline \n\rstring}", ShortbolParser.MultiLineLiteral)
        }

        'accepts - {
          * - shouldParse("{\nI am a string\n}", ShortbolParser.MultiLineLiteral, MultiLineLiteral("I am a string\n"::Nil, 0))
          * - shouldParse("{\n  I am a string\n  }", ShortbolParser.MultiLineLiteral, MultiLineLiteral("I am a string\n"::Nil, 2))
          * - shouldParse("{\n  I am a string\n }", ShortbolParser.MultiLineLiteral, MultiLineLiteral(" I am a string\n"::Nil, 1))
          * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParser.MultiLineLiteral, MultiLineLiteral("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1))
        }
      }

      'using_valueExp - {
        * - shouldParse("{\nI am a string\n}", ShortbolParser.ValueExp, MultiLineLiteral("I am a string\n"::Nil, 0))
        * - shouldParse("{\n  I am a string\n  }", ShortbolParser.ValueExp, MultiLineLiteral("I am a string\n"::Nil, 2))
        * - shouldParse("{\n  I am a string\n }", ShortbolParser.ValueExp, MultiLineLiteral(" I am a string\n"::Nil, 1))
        * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParser.ValueExp, MultiLineLiteral("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1))
      }
    }

    'IntegerLiteral - {
      'accepts - {
        * - shouldParse("0", ShortbolParser.IntegerLiteral, IntegerLiteral(0))
        * - shouldParse("123456789", ShortbolParser.IntegerLiteral, IntegerLiteral(123456789))
        * - shouldParse("+1", ShortbolParser.IntegerLiteral, IntegerLiteral(+1))
        * - shouldParse("-1", ShortbolParser.IntegerLiteral, IntegerLiteral(-1))
      }

      'rejects - {
        * - shouldNotParse("0.4", ShortbolParser.IntegerLiteral)
        * - shouldNotParse(".1", ShortbolParser.IntegerLiteral)
        * - shouldNotParse("a", ShortbolParser.IntegerLiteral)
      }

    }

    'Assignment - {
      'accepts - {
        * - shouldParse("x = y", ShortbolParser.Assignment, Assignment(LocalName("x"), LocalName("y")))
        * - shouldParse("sequence = \"aactaggactaatg\"", ShortbolParser.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
        * - shouldParse("sequence    =        \"aactaggactaatg\"", ShortbolParser.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
        * - shouldParse("encoding = <SBOL:DNA>", ShortbolParser.Assignment, Assignment(LocalName("encoding"), QName(NSPrefix("SBOL"), LocalName("DNA"))))
        * - shouldParse("type = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParser.Assignment, Assignment(LocalName("type"), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("<SBOL:DNA> = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParser.Assignment, Assignment(QName(NSPrefix("SBOL"), LocalName("DNA")), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("start =   22", ShortbolParser.Assignment, Assignment(LocalName("start"), IntegerLiteral(22)))
        * - shouldParse("component = DNAComponent", ShortbolParser.Assignment, Assignment(LocalName("component"), LocalName("DNAComponent")))
        * - shouldParse("sequence = {\naacc\naacc\n}", ShortbolParser.Assignment, Assignment(LocalName("sequence"), MultiLineLiteral("aacc\n"::"aacc\n"::Nil, 0)))
      }

      'rejects - {
        * - shouldNotParse("\"string\" = something", ShortbolParser.Assignment)
        * - shouldNotParse("22 = something", ShortbolParser.Assignment)
        * - shouldNotParse("sequence =", ShortbolParser.Assignment)
        * - shouldNotParse("sequence", ShortbolParser.Assignment)
      }
    }

    'Comment - {
      'accepts - {
        * - shouldParse("#this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~", ShortbolParser.Comment, Comment("this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~"))
        * - shouldParse("#", ShortbolParser.Comment, Comment(""))
      }

      'rejects - {
        * - shouldNotParse("this is a comment", ShortbolParser.Comment)
        * - shouldNotParse("#\n\r\r\ncomments can not span over multiple lines", ShortbolParser.Comment)
        * - shouldNotParse("#I should not be \n comment", ShortbolParser.Comment)
      }
    }

    'Import - {
      'accepts - {
        * - shouldParse("import template_libary", ShortbolParser.Import, Import("template_libary"))
        * - shouldParse("import /var/foo/libary/template_libary", ShortbolParser.Import, Import("/var/foo/libary/template_libary"))
      }

      'rejects - {
        * - shouldNotParse("importtemplate_libary", ShortbolParser.Import)
        * - shouldNotParse("import template\n_libary", ShortbolParser.Import)
      }
    }

    'Indent - {
      * - shouldParse(" ", parser.Indent, 1)
    }

    'IndentedInstanceBody - {
      * - shouldParse(
        "\n ", parser.IndentedInstanceBody,
        Seq(shortbol.BlankLine)
      )
    }

    'PrefixConstructorApp - {
      * - shouldParse("DNAComponent", parser.PrefixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("DNAComponent"), Nil
          ), Nil
        )
      )

      * - shouldParse(
        "DNAComponent\n ", parser.PrefixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("DNAComponent"), Nil
          ), Seq(shortbol.BlankLine)
        )
      )
    }


    'NestedInstance - {

      'accepts - {

        * - shouldParse("cds : DNAComponent", parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ), Nil
              )
            )
          )
        )

        * - shouldParse("cds:DNAComponent", parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ), Nil
              )
            )
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            | """.stripMargin, parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ), Seq(shortbol.BlankLine)
              )
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n \n ", parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ), Seq(shortbol.BlankLine, shortbol.BlankLine)
              )
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n   \n   \n   role = <SBOL:CDS>\n   \n   foo = bar\n   \n   ", parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ), Seq(
                  BlankLine,
                  BlankLine,
                  shortbol.Assignment(
                    LocalName("role"), QName(
                      NSPrefix("SBOL"), LocalName("CDS")
                    )
                  ),
                  BlankLine,
                  shortbol.Assignment(
                    LocalName("foo"), LocalName("bar")
                  ),
                  BlankLine,
                  BlankLine
                )
              )
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n   role = <SBOL:CDS>\n   \n   foo = bar\n   component : public",
          parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"),
                  Nil
                ),Seq(
                  shortbol.Assignment(
                    LocalName(
                      "role"),QName(
                      NSPrefix
                      ("SBOL"), LocalName("CDS")
                    )
                  ),
                  BlankLine,
                  shortbol.Assignment(LocalName("foo"),LocalName("bar")
                  ),
                  NestedInstance(
                    InstanceExp(
                      LocalName("component"),ConstructorApp(
                        TpeConstructor1(LocalName(
                          "public"),List()),List()))
                  )
                )
              )
            )
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            |   role = <SBOL:CDS>
            |   foo = bar
            |   component : public
            |      foo = bar
          """.
            stripMargin.
            trim,parser
            .
              NestedInstance, NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil
                ),
                Seq(
                  shortbol.
                    Assignment(
                      LocalName("role"),QName(
                        NSPrefix("SBOL"),
                        LocalName("CDS")
                      )
                    ),shortbol.
                    Assignment(LocalName("foo"),
                      LocalName("bar")
                    ),
                  NestedInstance(InstanceExp(
                    LocalName(
                      "component")
                    ,
                    ConstructorApp(
                      TpeConstructor1(
                        LocalName("public")
                        ,
                        List(
                        )),Seq(shortbol
                        .
                          Assignment(
                            LocalName("foo"),LocalName("bar")
                          )))
                  ))
                )
              )
            )
          )
        )
        * - shouldParse(
          """cds : DNAComponent
            |  role = <SBOL:CDS>""".stripMargin,parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"),Nil
                ),Seq(shortbol.Assignment(
                  LocalName("role"),QName(
                    NSPrefix("SBOL"),LocalName("CDS")
                  )
                ))
              )
            )
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            |  type = DNA
            |  role = <SBOL:CDS>""".stripMargin,parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"), ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"),Nil
                ),Seq(shortbol.Assignment(
                  LocalName("type"),LocalName("DNA")
                ),shortbol.Assignment(
                  LocalName("role"),QName(
                    NSPrefix("SBOL"),LocalName("CDS")
                  )
                ))
              )
            )
          )
        )

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
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

        * - shouldParse(
          """cds : DNAComponent
            |   role = <SBOL:CDS>
            |   foo = bar
            |   component : public
            |        foo = bar
          """.stripMargin.trim,
          parser.NestedInstance,
          NestedInstance(
            InstanceExp(
              LocalName("cds"),
              ConstructorApp(
                TpeConstructor1(
                  LocalName("DNAComponent"), Nil),
                Seq(
                  Assignment(LocalName("role"), QName(NSPrefix("SBOL"), LocalName("CDS"))),
                  Assignment(LocalName("foo"), LocalName("bar")),
                  NestedInstance(
                    InstanceExp(
                      LocalName("component"),
                      ConstructorApp(
                        TpeConstructor1(
                          LocalName("public"), Nil),
                        Seq(
                          Assignment(LocalName("foo"), LocalName("bar"))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }

      'rejects - {
        // template inside an instance constructor
        * - shouldNotParse(
          """cds : DNAComponent
            |  role = <SBOL:CDS>
            |  component => DNA""".stripMargin,parser.NestedInstance)

        //At a different nest.
        * - shouldNotParse(
          """cds : DNAComponent
            |   role = <SBOL:CDS>
            |      foo = bar
          """.stripMargin.trim,parser.NestedInstance
        )
      }
    }

    'InfixConstructorApp - {

      * - shouldParse(
        "a drives b", ShortbolParser.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("drives"),Seq(LocalName("a"),LocalName("b"))
          ),Nil
        )
      )

      * - shouldNotParse(
        "adrivesb", ShortbolParser.InfixConstructorApp)

      * - shouldNotParse(
        "1 is_not 2", ShortbolParser.InfixConstructorApp)

      * - shouldNotParse(
        "\"a\" drives \"b\"",ShortbolParser.InfixConstructorApp)

      * - shouldParse(
        "<SBOL:google> maps_to <www.google.co.uk>",ShortbolParser.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("maps_to"),Seq(QName(NSPrefix("SBOL"),LocalName("google")),Url("www.google.co.uk"))

          ),Nil
        )
      )

    }

    'ConstructorDef - {

      * - shouldParse(
        "DNAComponent => ComponentDefinition", parser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Nil
          )

        )
      )
      * - shouldParse(
        "DNAComponent=>ComponentDefinition", parser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse(
        "DNASequence(x) => Sequence", parser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse(
        "DNASequence  (x)  =>  Sequence", parser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse("a => b(x)", parser.ConstructorDef,
        ConstructorDef(LocalName("a"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("b"), Seq(LocalName("x"))

            ), Nil
          )

        )
      )
      * - shouldParse(
        """DNAComponent => ComponentDefinition
          |   type = DNA
        """.stripMargin.trim, parser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Seq(shortbol.Assignment(LocalName("type"), LocalName("DNA")))
          )

        )
      )

      * - shouldParse(
        """DNAComponent => ComponentDefinition
          |   type = DNA
          |   sequence : DNASequence
        """.stripMargin.trim, parser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Seq(shortbol.Assignment(LocalName("type"), LocalName("DNA")), NestedInstance(InstanceExp(LocalName("sequence"), ConstructorApp(TpeConstructor1(LocalName("DNASequence"), List()), List()))))
          )

        )
      )


    }

    'TopLevel - {

      * - shouldParse(
        "foo : bar", parser.TopLevel,
        InstanceExp(LocalName("foo"),
          ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
      )

      * - shouldParse(
        "foo => bar", parser.TopLevel,
        ConstructorDef(LocalName("foo"),
          Nil,
          ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
      )

      * - shouldParse(
        "import blablabla", parser.TopLevel,
        Import("blablabla")
      )

      * - shouldParse(
        "#comment", parser.TopLevel,
        Comment("comment"))

      * - shouldParse(
        "", parser.TopLevel,
        BlankLine
      )

      * - shouldParse(
        "x = y", ShortbolParser.Assignment,
        Assignment(LocalName("x"), LocalName("y")))

      * - shouldParse(
        "x = y", parser.TopLevel,
        Assignment(LocalName("x"), LocalName("y")))
    }

    'TopLevels - {

      * - shouldParse(
        """
          |
          |BBa_J61101_seq : DNASequence("aaagacaggacc")
          |  BBa_J61120_seq : DNASequence("aaagacaggacc")
          |    a = b
          |    #a equals b
          |    a = {
          |      helloswldk;lwdw;ldkw;kdw;kdw
          |      ldkw;lkdw;ldwk;dlwkdwjnkjkajfnwkjnaw
          |      }
          |
          |dna = c
          |
          |bbc = c""".stripMargin, parser.TopLevels
      )

      * - shouldParse(
        """
          |BBA_J61101_seq : DNASequence("aaaaa")
          |  a = b
          |  a = b
          |
          |BBa_J611210_seq : DNASequence("agcaaagc")
          |    a = b
          |    a = b""".stripMargin,parser.TopLevels
      )

      * - shouldParse(
        """
          |a = b
          |
          |
          |b = a
          |b = a
          |""".stripMargin,parser.TopLevels

      )

      * - shouldParse(
        """
          |seq => Sequence
          |  a = b
          |  a = b
          |
          |BBa_J611210_seq : seq""".stripMargin,parser.TopLevels

      )

    }

  }

}
