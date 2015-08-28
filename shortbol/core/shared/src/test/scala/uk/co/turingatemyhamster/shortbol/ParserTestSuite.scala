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

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }


  def shouldParse[T](txt: String, p: Parser[T], value: T): Unit = {
    //println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case s : Success[T] =>
        assert(s.value == value)
    }
  }

  def shouldParse[T](txt: String, p: Parser[T]): Unit = {
    //println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case s : Success[T] =>
        assert(true)
    }

  }

  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
    //println(escape(txt))
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
        * - shouldParse("anAlphaIdentifier", ShortbolParsers.LocalName, LocalName("anAlphaIdentifier"))
        * - shouldParse("a1234", ShortbolParsers.LocalName, LocalName("a1234"))
        * - shouldParse("_ajfh13", ShortbolParsers.LocalName, LocalName("_ajfh13"))
        * - shouldParse("a1234.abc-c", ShortbolParsers.LocalName, LocalName("a1234.abc-c"))
      }

      'rejects - {
        * - shouldNotParse("1abc", ShortbolParsers.LocalName)
        * - shouldNotParse(".abc1", ShortbolParsers.LocalName)
        * - shouldNotParse("-abc1", ShortbolParsers.LocalName)
      }
    }

    'NSPrefix - {
      'accepts - {
        * - shouldParse("anAlphaIdentifier", ShortbolParsers.NSPrefix, NSPrefix("anAlphaIdentifier"))
        * - shouldParse("a1234", ShortbolParsers.NSPrefix, NSPrefix("a1234"))
        * - shouldParse("_ajfh13", ShortbolParsers.NSPrefix, NSPrefix("_ajfh13"))
        * - shouldParse("a1234.abc-c", ShortbolParsers.NSPrefix, NSPrefix("a1234.abc-c"))
      }

      'rejects - {
        * - shouldNotParse("1abc", ShortbolParsers.NSPrefix)
        * - shouldNotParse(".abc1", ShortbolParsers.NSPrefix)
        * - shouldNotParse("-abc1", ShortbolParsers.NSPrefix)
      }
    }

    'QName - {
      'accepts - {
        * - shouldParse("a123:b234", ShortbolParsers.QName, QName(NSPrefix("a123"), LocalName("b234")))
        * - shouldParse("_a123.2:b234-5", ShortbolParsers.QName, QName(NSPrefix("_a123.2"), LocalName("b234-5")))
      }

      'rejects - {
        * - shouldNotParse("._a123.2:1b234-5", ShortbolParsers.QName)
        * - shouldNotParse("abc : cba ", ShortbolParsers.QName)
        * - shouldNotParse("abc :cba ", ShortbolParsers.QName)
        * - shouldNotParse("abc: cba ", ShortbolParsers.QName)
      }
    }

    'Url - {
      'accepts - {
        * - shouldParse(
          "a123_.-~",
          ShortbolParsers.Url,
          Url("a123_.-~"))
        * - shouldParse(
          "http://www.scala-lang.org/documentation/getting-started.html",
          ShortbolParsers.Url,
          Url("http://www.scala-lang.org/documentation/getting-started.html"))
      }

      'rejects - {
        * - shouldNotParse("<www.google.co.uk>", ShortbolParsers.Url)
      }
    }

    'StringLiteral - {
      'using_StringLiteral - {
        'accepts - {
          shouldParse(
            "\"I am a string with some special chars ~#¢∞^&*()£@!.\"",
            ShortbolParsers.StringLiteral,
            StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
        }

        'rejects - {
          * - shouldNotParse("\"I am a \"string\" with some special chars ~#¢∞^&*()£@!.\"", ShortbolParsers.StringLiteral)
          * - shouldNotParse("I am not a string", ShortbolParsers.StringLiteral)
          * - shouldNotParse("\"I am half a string", ShortbolParsers.StringLiteral)
          * - shouldNotParse("\"I am not\nastring\"", ShortbolParsers.StringLiteral)
        }
      }

      'using_ValueExp - {
        shouldParse(
          "\"I am a string with some special chars ~#¢∞^&*()£@!.\"",
          ShortbolParsers.ValueExp, StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
      }
    }

    'CurlyLiteral - {
      'rejects - {
        * - shouldNotParse("{I am a multiline \nstring}", ShortbolParsers.StringLiteral)
        * - shouldNotParse("{\nI am a string\n}", ShortbolParsers.StringLiteral)
        * - shouldNotParse("{\nI am\na string\n}", ShortbolParsers.StringLiteral)
      }

      'accepts - {
        * - shouldParse("{I am also a string}", ShortbolParsers.StringLiteral, StringLiteral("I am also a string", escaped = true))
        * - shouldParse("{ I am also a string }", ShortbolParsers.StringLiteral, StringLiteral(" I am also a string ", escaped = true))
      }
    }

    'MultiLineLiteral - {
      'using_MultiLineLiteral - {
        'rejects - {
          * - shouldNotParse("{I am a multiline \nstring}", ShortbolParsers.MultiLineLiteral)
          * - shouldNotParse("{I am a \nmultiline \n\rstring}", ShortbolParsers.MultiLineLiteral)
        }

        'accepts - {
          * - shouldParse("{\nI am a string\n}", ShortbolParsers.MultiLineLiteral, MultiLineLiteral("I am a string\n"::Nil, 0))
          * - shouldParse("{\n  I am a string\n  }", ShortbolParsers.MultiLineLiteral, MultiLineLiteral("I am a string\n"::Nil, 2))
          * - shouldParse("{\n  I am a string\n }", ShortbolParsers.MultiLineLiteral, MultiLineLiteral(" I am a string\n"::Nil, 1))
          * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParsers.MultiLineLiteral, MultiLineLiteral("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1))
        }
      }

      'using_valueExp - {
        * - shouldParse("{\nI am a string\n}", ShortbolParsers.ValueExp, MultiLineLiteral("I am a string\n"::Nil, 0))
        * - shouldParse("{\n  I am a string\n  }", ShortbolParsers.ValueExp, MultiLineLiteral("I am a string\n"::Nil, 2))
        * - shouldParse("{\n  I am a string\n }", ShortbolParsers.ValueExp, MultiLineLiteral(" I am a string\n"::Nil, 1))
        * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParsers.ValueExp, MultiLineLiteral("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1))
      }
    }

    'IntegerLiteral - {
      'accepts - {
        * - shouldParse("0", ShortbolParsers.IntegerLiteral, IntegerLiteral(0))
        * - shouldParse("123456789", ShortbolParsers.IntegerLiteral, IntegerLiteral(123456789))
        * - shouldParse("+1", ShortbolParsers.IntegerLiteral, IntegerLiteral(+1))
        * - shouldParse("-1", ShortbolParsers.IntegerLiteral, IntegerLiteral(-1))
      }

      'rejects - {
        * - shouldNotParse("0.4", ShortbolParsers.IntegerLiteral)
        * - shouldNotParse(".1", ShortbolParsers.IntegerLiteral)
        * - shouldNotParse("a", ShortbolParsers.IntegerLiteral)
      }

    }

    'Assignment - {
      'accepts - {
        * - shouldParse("x = y", ShortbolParsers.Assignment, Assignment(LocalName("x"), LocalName("y")))
        * - shouldParse("sequence = \"aactaggactaatg\"", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
        * - shouldParse("sequence    =        \"aactaggactaatg\"", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
        * - shouldParse("encoding = <SBOL:DNA>", ShortbolParsers.Assignment, Assignment(LocalName("encoding"), QName(NSPrefix("SBOL"), LocalName("DNA"))))
        * - shouldParse("type = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParsers.Assignment, Assignment(LocalName("type"), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("<SBOL:DNA> = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParsers.Assignment, Assignment(QName(NSPrefix("SBOL"), LocalName("DNA")), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("start =   22", ShortbolParsers.Assignment, Assignment(LocalName("start"), IntegerLiteral(22)))
        * - shouldParse("component = DNAComponent", ShortbolParsers.Assignment, Assignment(LocalName("component"), LocalName("DNAComponent")))
        * - shouldParse("sequence = {\naacc\naacc\n}", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), MultiLineLiteral("aacc\n"::"aacc\n"::Nil, 0)))
      }

      'rejects - {
        * - shouldNotParse("\"string\" = something", ShortbolParsers.Assignment)
        * - shouldNotParse("22 = something", ShortbolParsers.Assignment)
        * - shouldNotParse("sequence =", ShortbolParsers.Assignment)
        * - shouldNotParse("sequence", ShortbolParsers.Assignment)
      }
    }

    'Comment - {
      'accepts - {
        * - shouldParse("#this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~", ShortbolParsers.Comment, Comment("this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~"))
        * - shouldParse("#", ShortbolParsers.Comment, Comment(""))
      }

      'rejects - {
        * - shouldNotParse("this is a comment", ShortbolParsers.Comment)
        * - shouldNotParse("#\n\r\r\ncomments can not span over multiple lines", ShortbolParsers.Comment)
        * - shouldNotParse("#I should not be \n comment", ShortbolParsers.Comment)
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
      * - shouldParse(" ", ShortbolParser.Indent, 1)
    }

    'IndentedInstanceBody - {
      * - shouldParse(
        "\n ", ShortbolParser.IndentedInstanceBody,
        Seq(shortbol.BlankLine)
      )
    }

    'PrefixConstructorApp - {
      * - shouldParse("DNAComponent", ShortbolParser.PrefixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("DNAComponent"), Nil
          ), Nil
        )
      )

      * - shouldParse(
        "DNAComponent\n ", ShortbolParser.PrefixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("DNAComponent"), Nil
          ), Seq(shortbol.BlankLine)
        )
      )
    }


    'NestedInstance - {

      'accepts - {

        * - shouldParse("cds : DNAComponent", ShortbolParser.NestedInstance,
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

        * - shouldParse("cds:DNAComponent", ShortbolParser.NestedInstance,
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
            | """.stripMargin, ShortbolParser.NestedInstance,
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
          "cds : DNAComponent\n \n ", ShortbolParser.NestedInstance,
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
          "cds : DNAComponent\n   \n   \n   role = <SBOL:CDS>\n   \n   foo = bar\n   \n   ", ShortbolParser.NestedInstance,
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
          ShortbolParser.NestedInstance,
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
            trim,
          ShortbolParser.NestedInstance, NestedInstance(
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
            |  role = <SBOL:CDS>""".stripMargin,ShortbolParser.NestedInstance,
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
            |  role = <SBOL:CDS>""".stripMargin,
          ShortbolParser.NestedInstance,
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
          "dna_sequence : DNASequence(x)",
          ShortbolParser.NestedInstance,
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
          "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",
          ShortbolParser.NestedInstance,
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
          "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",
          ShortbolParser.NestedInstance,
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
          "i : Inline(20,50)",
          ShortbolParser.NestedInstance,
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
          "pass_qname : Test(<SBOL:DNA>)",
          ShortbolParser.NestedInstance,
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
          "pass_url : Test(<www.google.co.uk>)",
          ShortbolParser.NestedInstance,
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
          "url : <www.google.co.uk>",
          ShortbolParser.NestedInstance,
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
          "<SBOL:google> : <www.google.co.uk>",
          ShortbolParser.NestedInstance,
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
          "DNA : DNAComponent()",
          ShortbolParser.NestedInstance,
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
          ShortbolParser.NestedInstance,
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
            |  component => DNA""".stripMargin,
          ShortbolParser.NestedInstance)

        //At a different nest.
        * - shouldNotParse(
          """cds : DNAComponent
            |   role = <SBOL:CDS>
            |      foo = bar
          """.stripMargin.trim,
          ShortbolParser.NestedInstance
        )
      }
    }

    'InfixConstructorApp - {

      * - shouldParse(
        "a drives b", ShortbolParsers.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("drives"),Seq(LocalName("a"),LocalName("b"))
          ),Nil
        )
      )

      * - shouldNotParse(
        "adrivesb", ShortbolParsers.InfixConstructorApp)

      * - shouldNotParse(
        "1 is_not 2", ShortbolParsers.InfixConstructorApp)

      * - shouldNotParse(
        "\"a\" drives \"b\"",ShortbolParsers.InfixConstructorApp)

      * - shouldParse(
        "<SBOL:google> maps_to <www.google.co.uk>",ShortbolParsers.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("maps_to"),Seq(QName(NSPrefix("SBOL"),LocalName("google")),Url("www.google.co.uk"))

          ),Nil
        )
      )

    }

    'ConstructorDef - {

      * - shouldParse(
        "DNAComponent => ComponentDefinition", ShortbolParser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Nil
          )

        )
      )
      * - shouldParse(
        "DNAComponent=>ComponentDefinition", ShortbolParser.ConstructorDef,
        ConstructorDef(LocalName("DNAComponent"), Nil,
          ConstructorApp(
            TpeConstructor1(
              LocalName("ComponentDefinition"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse(
        "DNASequence(x) => Sequence", ShortbolParser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse(
        "DNASequence  (x)  =>  Sequence", ShortbolParser.ConstructorDef,
        ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
          ConstructorApp(
            TpeConstructor1(
              LocalName("Sequence"), Nil

            ), Nil
          )

        )
      )

      * - shouldParse("a => b(x)", ShortbolParser.ConstructorDef,
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
        """.stripMargin.trim, ShortbolParser.ConstructorDef,
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
        """.stripMargin.trim, ShortbolParser.ConstructorDef,
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
        "foo : bar", ShortbolParser.TopLevel,
        InstanceExp(LocalName("foo"),
          ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
      )

      * - shouldParse(
        "foo => bar", ShortbolParser.TopLevel,
        ConstructorDef(LocalName("foo"),
          Nil,
          ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
      )

      * - shouldParse(
        "import blablabla", ShortbolParser.TopLevel,
        Import("blablabla")
      )

      * - shouldParse(
        "#comment", ShortbolParser.TopLevel,
        Comment("comment"))

      * - shouldParse(
        "", ShortbolParser.TopLevel,
        BlankLine
      )

      * - shouldParse(
        "x = y", ShortbolParsers.Assignment,
        Assignment(LocalName("x"), LocalName("y")))

      * - shouldParse(
        "x = y", ShortbolParser.TopLevel,
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
          |bbc = c""".stripMargin, ShortbolParser.TopLevels
      )

      * - shouldParse(
        """
          |BBA_J61101_seq : DNASequence("aaaaa")
          |  a = b
          |  a = b
          |
          |BBa_J611210_seq : DNASequence("agcaaagc")
          |    a = b
          |    a = b""".stripMargin,ShortbolParser.TopLevels
      )

      * - shouldParse(
        """
          |a = b
          |
          |
          |b = a
          |b = a
          |""".stripMargin,ShortbolParser.TopLevels

      )

      * - shouldParse(
        """
          |seq => Sequence
          |  a = b
          |  a = b
          |
          |BBa_J611210_seq : seq""".stripMargin,ShortbolParser.TopLevels

      )

    }

  }

}
