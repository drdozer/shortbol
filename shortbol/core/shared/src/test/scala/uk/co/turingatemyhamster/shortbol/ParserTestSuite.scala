package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}
import uk.co.turingatemyhamster.shortbol
import uk.co.turingatemyhamster.shortbol.ast._
import uk.co.turingatemyhamster.shortbol.ops.{AllNodes, ShortbolParser, ShortbolParsers}
import utest._
import ast.sugar._
import ShortbolParser.POps

/**
  * Created by chris on 17/07/15.
  */
object ParserTestSuite extends TestSuite{

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }


  def shouldParse[T](txt: String, p: Parser[T], expected: T)(implicit an: AllNodes[T]): Unit = {
    //println(escape(txt))
    (Start ~ p ~ End).withPositions("_shouldParse_", txt) match {
      case s : Success[T] =>
        val observed = s.value
        assert(observed == expected)
        val without = AllNodes.in(observed) filter (_.region == null)
        assert(without.isEmpty)
    }
  }

  def shouldParse[T](txt: String, p: Parser[T]): Unit = {
    //println(escape(txt))
    (Start ~ p ~ End).withPositions("_shouldParse_", txt) match {
      case s : Success[T] =>
        assert(true)
    }

  }

  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
    //println(escape(txt))
    (Start ~ p ~ End).parse(txt) match {
      case f: Failure =>
        assert(true)
    }
  }

  /*scala test for automated testing - random leters ext */

  val tests = TestSuite {
    'identifiers - {
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

        'location - {
          val r = ShortbolParsers.LocalName.withPositions("_test_", "a1234").get.value.region
          assert(r == Region(startsAt = Pos(0, 1, 1),  endsAt = Pos(5, 1, 6), in = "_test_"))
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

        'location - {
          val r = ShortbolParsers.NSPrefix.withPositions("_test_", "LacR").get.value.region
          assert(r == Region(startsAt = Pos(0, 1, 1),  endsAt = Pos(4, 1, 5), in = "_test_"))
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

        'location - {
          val v = ShortbolParsers.QName.withPositions("_test_", "a123:b234").get.value
          val vr = v.region
          val vp = v.prefix.region
          val vl = v.localName.region

          * - assert(vr == Region(Pos(0, 1, 1), Pos(9, 1, 10), "_test_"))
          * - assert(vp == Region(Pos(0, 1, 1), Pos(4, 1, 5), "_test_"))
          * - assert(vl == Region(Pos(5, 1, 6), Pos(9, 1, 10), "_test_"))
        }
      }

      'Url - {
        'accepts - {
          * - shouldParse(
            "<a123_.-~>",
            ShortbolParsers.Url,
            Url("a123_.-~"))
          * - shouldParse(
            "<http://www.scala-lang.org/documentation/getting-started.html>",
            ShortbolParsers.Url,
            Url("http://www.scala-lang.org/documentation/getting-started.html"))
        }

        'rejects - {
          * - shouldNotParse("www.google.co.uk", ShortbolParsers.Url)
        }

        'location - {
          val v = ShortbolParsers.Url.withPositions("_test_", "<http://www.scala-lang.org/documentation/getting-started.html>").get.value.region
          assert(v == Region(Pos(0, 1, 1), Pos(62, 1, 63), "_test_"))
        }
      }
    }

    'StringLiteral - {
      'using_StringLiteral - {
        'accepts - {
          shouldParse(
            "\"I am a string with some special chars ~#¢∞^&*()£@!.\"",
            ShortbolParsers.QuotedStringLiteral,
            StringLiteral.SingleLine("I am a string with some special chars ~#¢∞^&*()£@!."))
        }

        'rejects - {
          * - shouldNotParse("\"I am a \"string\" with some special chars ~#¢∞^&*()£@!.\"", ShortbolParsers.QuotedStringLiteral)
          * - shouldNotParse("I am not a string", ShortbolParsers.QuotedStringLiteral)
          * - shouldNotParse("\"I am half a string", ShortbolParsers.QuotedStringLiteral)
          * - shouldNotParse("\"I am not\nastring\"", ShortbolParsers.QuotedStringLiteral)
        }

        'location - {
          val v = ShortbolParsers.QuotedStringLiteral.withPositions("_test_", "\"I am a string\"").get.value.region
          assert(v == Region(Pos(0, 1, 1), Pos(15,1,16), "_test_"))
        }
      }

      'using_ValueExp - {
        * - {
          shouldParse(
            "\"I am a string with some special chars ~#¢∞^&*()£@!.\"",
            ShortbolParsers.ValueExp, ValueExp.Literal(StringLiteral.SingleLine("I am a string with some special chars ~#¢∞^&*()£@!.")))
        }

        * - {
          val v0 = ShortbolParsers.ValueExp.withPositions("_test_", "\"I am a string\"").get.value
          val ValueExp.Literal(v1) = v0
          val v1r = v1.region

          assert(v1r == Region(Pos(0, 1, 1), Pos(15,1,16), "_test_"))
        }
      }
    }

    'CurlyLiteral - {
      'rejects - {
        * - shouldNotParse("{I am a multiline \nstring}", ShortbolParsers.CurlyStringLiteral)
        * - shouldNotParse("{\nI am a string\n}", ShortbolParsers.CurlyStringLiteral)
        * - shouldNotParse("{\nI am\na string\n}", ShortbolParsers.CurlyStringLiteral)
      }

      'accepts - {
        * - shouldParse("{I am also a string}", ShortbolParsers.CurlyStringLiteral, StringLiteral.SingleLine("I am also a string", escaped = true))
        * - shouldParse("{ I am also a string }", ShortbolParsers.CurlyStringLiteral, StringLiteral.SingleLine(" I am also a string ", escaped = true))
      }

      'location - {
        val r = ShortbolParsers.CurlyStringLiteral.withPositions("_test_", "{I am also a string}").get.value.region
        assert(r == Region(Pos(0, 1, 1), Pos(20, 1, 21), "_test_"))
      }
    }

    'MultiLineLiteral - {
      'using_MultiLineLiteral - {
        'rejects - {
          * - shouldNotParse("{I am a multiline \nstring}", ShortbolParsers.MultiLineLiteral)
          * - shouldNotParse("{I am a \nmultiline \n\rstring}", ShortbolParsers.MultiLineLiteral)
        }

        'accepts - {
          * - shouldParse("{\nI am a string\n}", ShortbolParsers.MultiLineLiteral, StringLiteral.MultiLine("I am a string\n"::Nil, 0))
          * - shouldParse("{\n  I am a string\n  }", ShortbolParsers.MultiLineLiteral, StringLiteral.MultiLine("I am a string\n"::Nil, 2))
          * - shouldParse("{\n  I am a string\n }", ShortbolParsers.MultiLineLiteral, StringLiteral.MultiLine(" I am a string\n"::Nil, 1))
          * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParsers.MultiLineLiteral, StringLiteral.MultiLine("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1))
        }

        'location - {
          val r = ShortbolParsers.MultiLineLiteral.withPositions("_test_", "{\nI am a string\n}").get.value.region
          assert(r == Region(Pos(0, 1, 1), Pos(17, 3, 2), "_test_"))
        }
      }

      'using_valueExp - {
        * - shouldParse("{\nI am a string\n}", ShortbolParsers.ValueExp, ValueExp.Literal(StringLiteral.MultiLine("I am a string\n"::Nil, 0)))
        * - shouldParse("{\n  I am a string\n  }", ShortbolParsers.ValueExp, ValueExp.Literal(StringLiteral.MultiLine("I am a string\n"::Nil, 2)))
        * - shouldParse("{\n  I am a string\n }", ShortbolParsers.ValueExp, ValueExp.Literal(StringLiteral.MultiLine(" I am a string\n"::Nil, 1)))
        * - shouldParse("{\n I\n Am\n A\n Typeface\n }", ShortbolParsers.ValueExp, ValueExp.Literal(StringLiteral.MultiLine("I\n"::"Am\n"::"A\n"::"Typeface\n"::Nil, 1)))
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
        * - shouldParse("sequence = \"aactaggactaatg\"", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), StringLiteral.SingleLine("aactaggactaatg")))
        * - shouldParse("sequence    =        \"aactaggactaatg\"", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), StringLiteral.SingleLine("aactaggactaatg")))
        * - shouldParse("encoding = SBOL:DNA", ShortbolParsers.Assignment, Assignment(LocalName("encoding"), QName(NSPrefix("SBOL"), LocalName("DNA"))))
        * - shouldParse("type = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParsers.Assignment, Assignment(LocalName("type"), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("SBOL:DNA = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", ShortbolParsers.Assignment, Assignment(QName(NSPrefix("SBOL"), LocalName("DNA")), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
        * - shouldParse("start =   22", ShortbolParsers.Assignment, Assignment(LocalName("start"), IntegerLiteral(22)))
        * - shouldParse("component = DNAComponent", ShortbolParsers.Assignment, Assignment(LocalName("component"), LocalName("DNAComponent")))
        * - shouldParse("sequence = {\naacc\naacc\n}", ShortbolParsers.Assignment, Assignment(LocalName("sequence"), StringLiteral.MultiLine("aacc\n"::"aacc\n"::Nil, 0)))
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

    'Indent - {
      * - shouldParse(" ", ShortbolParser.Indent, 1)
    }

    'IndentedInstanceBody - {
      * - shouldParse(
        "\n ", ShortbolParser.IndentedInstanceBody,
        Seq(ast.BlankLine() : BodyStmt)
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
          ), Seq(ast.BlankLine() : BodyStmt)
        )
      )
    }

    'InstanceExp - {
      * - shouldParse(
        "foo : bar", ShortbolParser.InstanceExp,
        InstanceExp(LocalName("foo"),
          ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
      )

      * - shouldParse("cds : DNAComponent", ShortbolParser.InstanceExp,
        InstanceExp(
          LocalName("cds"), ConstructorApp(
            TpeConstructor1(
              LocalName("DNAComponent"), Nil
            ), Nil
          )
        )
      )
    }

    'InstanceExp - {

      'accepts - {

        * - shouldParse("cds : DNAComponent", ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil
              ), Nil
            )
          )
        )

        * - shouldNotParse("cds:DNAComponent", ShortbolParser.InstanceExp)

        * - shouldParse(
          """cds : DNAComponent
            | """.stripMargin, ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil
              ), Seq(ast.BlankLine())
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n \n ", ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil
              ), Seq(ast.BlankLine(), ast.BlankLine())
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n   \n   \n   role = SBOL:CDS\n   \n   foo = bar\n   \n   ", ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil
              ), Seq(
                BlankLine(),
                BlankLine(),
                Assignment(
                  LocalName("role"), QName(
                    NSPrefix("SBOL"), LocalName("CDS")
                  )
                ),
                BlankLine(),
                Assignment(
                  LocalName("foo"), LocalName("bar")
                ),
                BlankLine(),
                BlankLine()
              )
            )
          )
        )

        * - shouldParse(
          "cds : DNAComponent\n   role = SBOL:CDS\n   \n   foo = bar\n   component : public",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),
                Nil
              ),Seq(
                Assignment(
                  LocalName(
                    "role"),QName(
                    NSPrefix
                    ("SBOL"), LocalName("CDS")
                  )
                ),
                BlankLine(),
                Assignment(LocalName("foo"),LocalName("bar")
                ),
                InstanceExp(
                  LocalName("component"),ConstructorApp(
                    TpeConstructor1(LocalName(
                      "public"),List()),List()))
              )
            )
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            |   role = SBOL:CDS
            |   foo = bar
            |   component : public
            |      foo = bar
          """.
            stripMargin.
            trim,
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil
              ),
              Seq(
                Assignment(
                  LocalName("role"),QName(
                    NSPrefix("SBOL"),
                    LocalName("CDS")
                  )
                ),ast.Assignment(LocalName("foo"),
                  LocalName("bar")
                ),
                InstanceExp(
                  LocalName(
                    "component")
                  ,
                  ConstructorApp(
                    TpeConstructor1(
                      LocalName("public")
                      ,
                      List(
                      )),Seq(Assignment(
                      LocalName("foo"),LocalName("bar")
                    )))
                )
              )
            )
          )
        )
        * - shouldParse(
          """cds : DNAComponent
            |  role = SBOL:CDS""".stripMargin,ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil
              ),Seq(ast.Assignment(
                LocalName("role"),QName(
                  NSPrefix("SBOL"),LocalName("CDS")
                )
              ))
            )
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            |  type = DNA
            |  role = SBOL:CDS""".stripMargin,
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil
              ),Seq(ast.Assignment(
                LocalName("type"),LocalName("DNA")
              ),ast.Assignment(
                LocalName("role"),QName(
                  NSPrefix("SBOL"),LocalName("CDS")
                )
              ))
            )
          )
        )

        * - shouldParse(
          "dna_sequence : DNASequence(x)",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(LocalName("x"))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(StringLiteral.SingleLine("AAAAGTAAAACA"))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("dna_sequence"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNASequence"),Seq(StringLiteral.SingleLine("AAAAGTAAAACA"))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "i : Inline(20,50)",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("i"), ConstructorApp(
              TpeConstructor1(
                LocalName("Inline"),Seq(IntegerLiteral(20),IntegerLiteral(50))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "pass_qname : Test(SBOL:DNA)",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("pass_qname"), ConstructorApp(
              TpeConstructor1(
                LocalName("Test"),Seq(QName(NSPrefix("SBOL"),LocalName("DNA")))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "pass_url : Test(<www.google.co.uk>)",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("pass_url"), ConstructorApp(
              TpeConstructor1(
                LocalName("Test"),Seq(Url("www.google.co.uk"))
              ),Nil
            )
          )
        )

        * - shouldParse(
          "url : <www.google.co.uk>",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("url"), ConstructorApp(
              TpeConstructor1(
                Url("www.google.co.uk"),Nil)
              ,Nil)
          )
        )

        * - shouldParse(
          "SBOL:google : <www.google.co.uk>",
          ShortbolParser.InstanceExp,
          InstanceExp(
            QName(NSPrefix("SBOL"),LocalName("google")), ConstructorApp(
              TpeConstructor1(
                Url("www.google.co.uk"),Nil)
              ,Nil)
          )
        )

        * - shouldParse(
          "DNA : DNAComponent()",
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("DNA"), ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"),Nil)
              ,Nil)
          )
        )

        * - shouldParse(
          """cds : DNAComponent
            |   role = SBOL:CDS
            |   foo = bar
            |   component : public
            |        foo = bar
          """.stripMargin.trim,
          ShortbolParser.InstanceExp,
          InstanceExp(
            LocalName("cds"),
            ConstructorApp(
              TpeConstructor1(
                LocalName("DNAComponent"), Nil),
              Seq(
                Assignment(LocalName("role"), QName(NSPrefix("SBOL"), LocalName("CDS"))),
                Assignment(LocalName("foo"), LocalName("bar")),
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

        * - shouldParse(
          """a : b
            |c : d""".stripMargin,
          ShortbolParser.TopLevels,
          Seq(
            TopLevel.InstanceExp(
              InstanceExp("a", ConstructorApp(
                TpeConstructor1("b", Seq()),
                Seq()
              ))
            ),
            TopLevel.InstanceExp(
              InstanceExp("c", ConstructorApp(
                TpeConstructor1("d", Seq()),
                Seq()
              ))
            )
          )
        )

      }

      'rejects - {
        // template inside an instance constructor
        * - shouldNotParse(
          """cds : DNAComponent
            |  role = SBOL:CDS
            |  component => DNA""".stripMargin,
          ShortbolParser.InstanceExp)

        //At a different nest.
        * - shouldNotParse(
          """cds : DNAComponent
            |   role = SBOL:CDS
            |      foo = bar
          """.stripMargin.trim,
          ShortbolParser.InstanceExp
        )

        * - shouldNotParse(
        """me : you
          |  name""".stripMargin,
          ShortbolParser.InstanceExp)

        * - shouldNotParse(
        """me : you
          |  name
          |""".stripMargin,
          ShortbolParser.InstanceExp)
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

      * - shouldParse(
        "1 is_not 2", ShortbolParsers.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("is_not"), Seq(IntegerLiteral(1), IntegerLiteral(2))
          ), Nil
        ))

      * - shouldParse(
        "\"a\" drives \"b\"",ShortbolParsers.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("drives"), Seq(StringLiteral.SingleLine("a"), StringLiteral.SingleLine("b"))
          ), Nil
        ))

      * - shouldParse(
        "SBOL:google maps_to <www.google.co.uk>",ShortbolParsers.InfixConstructorApp,
        ConstructorApp(
          TpeConstructor1(
            LocalName("maps_to"),Seq(QName(NSPrefix("SBOL"),LocalName("google")),Url("www.google.co.uk"))

          ),Nil
        )
      )

    }

    'ConstructorDef - {

      * - shouldParse(
        "DNAComponent => ComponentDefinition", ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNAComponent"), Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("ComponentDefinition"), Nil

              ), Nil
            )
          )
        )
      )
      * - shouldParse(
        "DNAComponent=>ComponentDefinition", ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNAComponent"), Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("ComponentDefinition"), Nil

              ), Nil
            )
          )
        )
      )

      * - shouldParse(
        "DNASequence(x) => Sequence", ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNASequence"), Seq(LocalName("x")),
            ConstructorApp(
              TpeConstructor1(
                LocalName("Sequence"), Nil

              ), Nil
            )
          )
        )
      )

      * - shouldParse(
        "DNASequence  (x)  =>  Sequence", ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNASequence"), Seq(LocalName("x")),
            ConstructorApp(
              TpeConstructor1(
                LocalName("Sequence"), Nil

              ), Nil
            )
          )
        )
      )

      * - shouldParse("a => b(x)", ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("a"), Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("b"), Seq(LocalName("x"))

              ), Nil
            )
          )
        )
      )
      * - shouldParse(
        """DNAComponent => ComponentDefinition
          |   type = DNA
        """.stripMargin.trim, ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNAComponent"), Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("ComponentDefinition"), Nil

              ), Seq(BodyStmt.Assignment(Assignment(LocalName("type"), LocalName("DNA"))))
            )
          )
        )
      )

      * - shouldParse(
        """DNAComponent => ComponentDefinition
          |   type = DNA
          |   sequence : DNASequence
        """.stripMargin.trim, ShortbolParser.topLevel.ConstructorDef,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("DNAComponent"), Nil,
            ConstructorApp(
              TpeConstructor1(
                LocalName("ComponentDefinition"), Nil
              ), Seq(ast.Assignment(LocalName("type"), LocalName("DNA")),
                InstanceExp(LocalName("sequence"), ConstructorApp(TpeConstructor1(LocalName("DNASequence"), List()), List())))
            )
          )
        )
      )

      * - shouldParse(
        """WithNameAge(name, age) => WithAge(age)
          |  foaf:name = name
        """.stripMargin.trim, ShortbolParser.ConstructorDef)

      * - shouldParse(
        """WithNameAge(name, age) => WithAge(age)
          |  foaf:name = name
        """.stripMargin.trim, ShortbolParser.topLevel.ConstructorDef)


    }

    'TopLevel - {

      * - shouldParse(
        "foo : bar", ShortbolParser.TopLevel,
        TopLevel.InstanceExp(
          InstanceExp(
            LocalName("foo"),
           ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil)
          )
        )
      )(AllNodes.topLevel)

      * - shouldParse(
        "foo => bar", ShortbolParser.TopLevel,
        TopLevel.ConstructorDef(
          ConstructorDef(
            LocalName("foo"),
            Nil,
            ConstructorApp(TpeConstructor1(LocalName("bar"), Nil), Nil))
        )
      )

      * - shouldParse(
        "#comment", ShortbolParser.TopLevel,
        TopLevel.Comment(Comment("comment")))

      * - shouldParse(
        "", ShortbolParser.TopLevel,
        TopLevel.BlankLine(BlankLine())
      )

      * - shouldParse(
        "x = y", ShortbolParsers.Assignment,
        Assignment(LocalName("x"), LocalName("y")))

      * - shouldParse(
        "x = y", ShortbolParser.TopLevel,
        TopLevel.Assignment(Assignment(LocalName("x"), LocalName("y"))))
    }

    'Pragma - {
      * - shouldParse(
        "@import <http://foo.bar.com/myLibrary>",
        ShortbolParsers.Pragma,
        Pragma(id = "import", values = Seq(Url("http://foo.bar.com/myLibrary")))
      )
    }

    'TopLevels - {

      * - {
        shouldParse(
          """@prefix foo <http://some.com/stuff#>
            |foo:me : foaf:person""".stripMargin,
          ShortbolParser.TopLevels)
      }

      * - {
        shouldParse(
          "foo:me : foaf:person",
          ShortbolParser.TopLevels)
      }

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
          |    a = b""".stripMargin, ShortbolParser.TopLevels
      )

      * - shouldParse(
        """
          |a = b
          |
          |
          |b = a
          |b = a
          |""".stripMargin, ShortbolParser.TopLevels

      )

      * - shouldParse(
        """
          |seq => Sequence
          |  a = b
          |  a = b
          |
          |BBa_J611210_seq : seq""".stripMargin, ShortbolParser.TopLevels

      )
    }

    'SBFile - {
      * - shouldParse(
        """WithNameAge(name, age) => WithAge(age)
          |  foaf:name = name
          |
          |WithAge(age) => foaf:person
          |  foaf:age = age
          |
          |me : WithNameAge("matthew", 40)
          |  foaf:knows = "caroline"
          |""".stripMargin, ShortbolParser.SBFile)

      * - shouldParse(
        """me : foaf:person
           |  foaf:age = 40
           |  foaf:name = "matthew"
           |  foaf:knows = "caroline"
           |""".stripMargin, ShortbolParser.SBFile)
    }

  }

}
