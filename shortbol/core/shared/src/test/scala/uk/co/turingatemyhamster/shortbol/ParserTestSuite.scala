//package uk.co.turingatemyhamster.shortbol
//
//import fastparse.Parser
//import fastparse.core.Result
//import fastparse.parsers.Terminals.{End, Start}
//import uk.co.turingatemyhamster.shortbol
//import utest._
//
///**
// * Created by chris on 17/07/15.
// */
//object ParserTestSuite extends TestSuite{
//  val parser = new ShortbolParser()
//
//  def escape(raw: String): String = {
//    import scala.reflect.runtime.universe._
//    Literal(Constant(raw)).toString
//  }
//
//
//  def shouldParse[T](txt: String, p: Parser[T], value: T): Unit = {
//    println(escape(txt))
//    (Start ~ p ~ End log "shouldParse").parse(txt) match {
//      case s : Result.Success.Mutable[T] =>
//        assert(s.value == value)
//    }
//  }
//
//  def quickTopLevelShouldParse[T](txt: String, p: Parser[T]): Unit = {
//    println(escape(txt))
//    (Start ~ p ~ End log "shouldParse").parse(txt + "\n") match {
//      case s : Result.Success.Mutable[T] =>
//        assert(true)
//    }
//
//  }
//
//  def shouldNotParse[T](txt: String, p: Parser[T]): Unit = {
//    println(escape(txt))
//    (Start ~ p ~ End log "shouldNotParse").parse(txt) match {
//      case s : Result.Success.Mutable[T] =>
//        assert(false)
//
//      case f: Result.Failure.Mutable =>
//        assert(true)
//    }
//  }
//
//  /*scala test for automated testing - random leters ext */
//
//  val tests = TestSuite {
////
//    'LocalName - {
//      shouldParse("anAlphaIdentifier", parser.LocalName, LocalName("anAlphaIdentifier"))
//      shouldParse("a1234", parser.LocalName, LocalName("a1234"))
//      shouldParse("_ajfh13", parser.LocalName, LocalName("_ajfh13"))
//      shouldParse("a1234.abc-c", parser.LocalName, LocalName("a1234.abc-c"))
//      shouldNotParse("1abc", parser.LocalName)
//      shouldNotParse(".abc1", parser.LocalName)
//      shouldNotParse("-abc1", parser.LocalName)
//    }
//
//    'NSPrefix - {
//      shouldParse("anAlphaIdentifier", parser.NSPrefix, NSPrefix("anAlphaIdentifier"))
//      shouldParse("a1234", parser.NSPrefix, NSPrefix("a1234"))
//      shouldParse("_ajfh13", parser.NSPrefix, NSPrefix("_ajfh13"))
//      shouldParse("a1234.abc-c", parser.NSPrefix, NSPrefix("a1234.abc-c"))
//      shouldNotParse("1abc", parser.NSPrefix)
//      shouldNotParse(".abc1", parser.NSPrefix)
//      shouldNotParse("-abc1", parser.NSPrefix)
//    }
//
//    'QName - {
//
//      shouldParse("a123:b234", parser.QName, QName(NSPrefix("a123"), LocalName("b234")))
//      shouldParse("_a123.2:b234-5", parser.QName, QName(NSPrefix("_a123.2"), LocalName("b234-5")))
//      shouldNotParse("._a123.2:1b234-5", parser.QName)
//      shouldNotParse("abc : cba ", parser.QName)
//      shouldNotParse("abc :cba ", parser.QName)
//      shouldNotParse("abc: cba ", parser.QName)
//
//    }
//    'Url - {
//
//      shouldParse("a123_.-~", parser.Url, Url("a123_.-~"))
//      shouldParse("http://www.scala-lang.org/documentation/getting-started.html", parser.Url, Url("http://www.scala-lang.org/documentation/getting-started.html"))
//      shouldNotParse("<www.google.co.uk>", parser.Url)
//    }
//
//    'StringLiteral - {
//      shouldParse("\"I am a string with some special chars ~#¢∞^&*()£@!.\"", parser.StringLiteral, StringLiteral("I am a string with some special chars ~#¢∞^&*()£@!."))
//      shouldNotParse("\"I am a \"string\" with some special chars ~#¢∞^&*()£@!.\"", parser.StringLiteral)
//      shouldNotParse("I am not a string", parser.StringLiteral)
//      shouldNotParse("\"I am half a string", parser.StringLiteral)
//      shouldNotParse("\"I am not\nastring\"", parser.StringLiteral)
//      shouldParse("{I am also a string}", parser.StringLiteral, StringLiteral("I am also a string"))
//      shouldParse("{ I am also a string }", parser.StringLiteral, StringLiteral(" I am also a string "))
//      shouldParse("{I am a multiline \nstring}", parser.StringLiteral, StringLiteral("I am a multiline string"))
//      shouldParse("{I am a \nmultiline \n\rstring}", parser.StringLiteral, StringLiteral("I am a multiline string"))
//      shouldNotParse("{{I am not a string}", parser.StringLiteral)
//      shouldParse("\'I am a single quote string\'", parser.StringLiteral, StringLiteral("I am a single quote string"))
//
//    }
//
//    'IntegerLiteral - {
//
//      shouldParse("123456789", parser.IntegerLiteral, IntegerLiteral(123456789))
//      shouldNotParse("0.4", parser.IntegerLiteral)
//      //      shouldParse("-2",parser.IntegerLiteral,IntegerLiteral(-2)) /*??? cant see where negative integers are needed.*/
//      //      shouldNotParse("abc123",parser.IntegerLiteral,IntegerLiteral(abc123))
//
//    }
//
//    'Assignment - {
//
//      shouldParse("sequence = \"aactaggactaatg\"", parser.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
//      shouldParse("sequence    =        \"aactaggactaatg\"", parser.Assignment, Assignment(LocalName("sequence"), StringLiteral("aactaggactaatg")))
//      shouldParse("encoding = <SBOL:DNA>", parser.Assignment, Assignment(LocalName("encoding"), QName(NSPrefix("SBOL"), LocalName("DNA"))))
//      shouldParse("type = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", parser.Assignment, Assignment(LocalName("type"), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
//      shouldParse("<SBOL:DNA> = <http://www.biopax.org/release/biopax-level3.owl#DnaRegion>", parser.Assignment, Assignment(QName(NSPrefix("SBOL"), LocalName("DNA")), Url("http://www.biopax.org/release/biopax-level3.owl#DnaRegion")))
//      shouldParse("start =   22", parser.Assignment, Assignment(LocalName("start"), IntegerLiteral(22)))
//      shouldParse("component = DNAComponent", parser.Assignment, Assignment(LocalName("component"), LocalName("DNAComponent")))
//      shouldNotParse("\"string\" = something", parser.Assignment)
//      shouldNotParse("22 = something", parser.Assignment)
//      shouldNotParse("sequence =", parser.Assignment)
//      shouldNotParse("sequence", parser.Assignment)
//      shouldParse("sequence = {aacc\naacc}", parser.Assignment, Assignment(LocalName("sequence"), StringLiteral("aaccaacc")))
//    }
//
//    'Comment - {
//
//      shouldParse("#this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~", parser.Comment, Comment("this is a comment, with some random chars €!@£$$%^&*(){}\"\"/'<>~"))
//      shouldNotParse("this is a comment", parser.Comment)
//      shouldParse("#", parser.Comment, Comment(""))
//
//      shouldNotParse("#\n\r\r\ncomments can not span over multiple lines", parser.Comment)
//      shouldNotParse("#I should not be \n comment", parser.Comment)
//    }
//
//    'Import - {
//
//      shouldParse("import template_libary", parser.Import, Import("template_libary"))
//      shouldParse("import /var/foo/libary/template_libary", parser.Import, Import("/var/foo/libary/template_libary"))
//      shouldNotParse("importtemplate_libary", parser.Import)
//      shouldNotParse("import template\n_libary", parser.Import)
//    }
//
//    'NestedInstance - {
//
//      shouldParse("cds : DNAComponent", parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"), Nil
//              ), Nil
//            )
//          )
//        )
//      )
//      shouldParse("cds:DNAComponent", parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"), Nil
//              ), Nil
//            )
//          )
//        )
//      )
//
//      shouldParse(
//        """cds : DNAComponent
//          | """.stripMargin, parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"), Nil
//              ), Seq(shortbol.BlankLine)
//            )
//          )
//        )
//      )
//      shouldParse(
//        """cds : DNAComponent
//          |
//          | """.stripMargin, parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"), Nil
//              ), Seq(shortbol.BlankLine)
//            )
//          )
//        )
//      )
//
//      shouldParse(
//        """cds : DNAComponent
//          |
//          |
//          |   role = <SBOL:CDS>
//          |
//          |   foo = bar
//          |
//          |
//        """.stripMargin.trim, parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"), Nil
//              ), Seq(shortbol.Assignment(
//                LocalName("role"), QName(
//                  NSPrefix("SBOL"), LocalName("CDS")
//                )
//              ), shortbol.Assignment(
//                LocalName("foo"), LocalName("bar")
//              ))
//            )
//          )
//        )
//      )
//
//      shouldParse(
//        """cds : DNAComponent
//                |   role = <SBOL:CDS>
//                |
//                |   foo = bar
//                |   component : public""".stripMargin, parser.NestedInstance,
//        NestedInstance(
//          InstanceExp(
//            LocalName("cds"), ConstructorApp(
//              TpeConstructor1(
//                LocalName("DNAComponent"),
//                Nil
//                    )
//              ,Seq(shortbol.Assignment(
//                LocalName(
//                  "role"),QName(
//                        NSPrefix
//                          ("SBOL"), LocalName("CDS")
//                      )
//                    ),
//                shortbol.Assignment(LocalName("foo"),LocalName("bar")
//                    ),
//                      NestedInstance(
//                        InstanceExp(
//                          LocalName("component"),ConstructorApp(
//                            TpeConstructor1(LocalName(
//                              "public"),List()),List()))
//                      )
//                    )
//                  )
//                )
//              )
//
//      )
//      shouldParse(
//        """cds : DNAComponent
//          |   role = <SBOL:CDS>
//                |   foo = bar
//                |   component : public
//                |      foo = bar
//              """.
//                stripMargin.
//                trim,parser
//                .
//                NestedInstance, NestedInstance(
//                InstanceExp(
//                  LocalName("cds"), ConstructorApp(
//                    TpeConstructor1(
//                      LocalName("DNAComponent"), Nil
//                    ),
//                    Seq(
//                      shortbol.
//                        Assignment(
//                      LocalName("role"),QName(
//                        NSPrefix("SBOL"),
//                        LocalName("CDS")
//                      )
//                    ),shortbol.
//                        Assignment(LocalName("foo"),
//                          LocalName("bar")
//                    ),
//                      NestedInstance(InstanceExp(
//                        LocalName(
//                      "component")
//                        ,
//                        ConstructorApp(
//                          TpeConstructor1(
//                        LocalName("public")
//                        ,
//                        List(
//                        )),Seq(shortbol
//                          .
//                          Assignment(
//                      LocalName("foo"),LocalName("bar")
//                      )))
//                      ))
//                    )
//                  )
//                )
//              )
//            )
//
//                  shouldNotParse(  //2nd nested indent is not consistent with the first.
//              """cds : DNAComponent
//                      |   role = <SBOL:CDS>
//                      |   foo = bar
//                      |   component : public
//                      |        foo = bar
//                    """.stripMargin.trim,parser.NestedInstance
//
//            )
//
//
//
//                  shouldNotParse(
//                    """cds : DNAComponent
//                      |  role = <SBOL:CDS>
//                      |  component => DNA""".stripMargin,parser.NestedInstance)
//
//                  //At a different nest.
//            shouldNotParse(
//                    """cds : DNAComponent
//                      |   role = <SBOL:CDS>
//                      |      foo = bar
//                    """.stripMargin.trim,parser.NestedInstance
//                  )
//
//                  shouldParse(
//                    """cds : DNAComponent
//                      |  role = <SBOL:CDS>""".stripMargin,parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("cds"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("DNAComponent"),Nil
//                          ),Seq(shortbol.Assignment(
//                            LocalName("role"),QName(
//                              NSPrefix("SBOL"),LocalName("CDS")
//                            )
//                          ))
//                        )
//                )
//                    )
//            )
//
//            shouldParse(
//                    """cds : DNAComponent
//                      |  type = DNA
//                      |  role = <SBOL:CDS>""".stripMargin,parser.NestedInstance,
//              NestedInstance(
//                      InstanceExp(
//                        LocalName("cds"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("DNAComponent"),Nil
//                          ),Seq(shortbol.Assignment(
//                            LocalName("type"),LocalName("DNA")
//                            ),shortbol.Assignment(
//                            LocalName("role"),QName(
//                              NSPrefix("SBOL"),LocalName("CDS")
//                            )
//                    ))
//                        )
//                )
//              )
//                  )
//
//                  shouldParse(
//                    "dna_sequence : DNASequence(x)",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                  LocalName("dna_sequence"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("DNASequence"),Seq(LocalName("x"))
//                          ),Nil
//                  )
//                      )
//              )
//            )
//
//            shouldParse(
//                    "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",parser.NestedInstance,
//              NestedInstance(
//                      InstanceExp(
//                        LocalName("dna_sequence"), ConstructorApp(
//                    TpeConstructor1(
//                            LocalName("DNASequence"),Seq(StringLiteral("AAAAGTAAAACA"))
//                          ),Nil
//                        )
//                      )
//                    )
//            )
//
//            shouldParse(
//                    "dna_sequence : DNASequence(\"AAAAGTAAAACA\")",parser.NestedInstance,
//                    NestedInstance(
//                InstanceExp(
//                        LocalName("dna_sequence"), ConstructorApp(
//                    TpeConstructor1(
//                            LocalName("DNASequence"),Seq(StringLiteral("AAAAGTAAAACA"))
//                          ),Nil
//                        )
//                      )
//                    )
//            )
//
//            shouldParse(
//                    "i : Inline(20,50)",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("i"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("Inline"),Seq(IntegerLiteral(20),IntegerLiteral(50))
//                          ),Nil
//                        )
//                      )
//                    )
//            )
//
//                  shouldParse(
//                    "pass_qname : Test(<SBOL:DNA>)",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("pass_qname"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("Test"),Seq(QName(NSPrefix("SBOL"),LocalName("DNA")))
//                          ),Nil
//                        )
//                      )
//                    )
//                  )
//
//                  shouldParse(
//                    "pass_url : Test(<www.google.co.uk>)",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("pass_url"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("Test"),Seq(Url("www.google.co.uk"))
//                          ),Nil
//                        )
//                      )
//                    )
//                  )
//
//                  shouldParse(
//                    "url : <www.google.co.uk>",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("url"), ConstructorApp(
//                          TpeConstructor1(
//                            Url("www.google.co.uk"),Nil)
//                          ,Nil)
//                      )
//                    )
//                  )
//
//                  shouldParse(
//                    "<SBOL:google> : <www.google.co.uk>",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        QName(NSPrefix("SBOL"),LocalName("google")), ConstructorApp(
//                          TpeConstructor1(
//                            Url("www.google.co.uk"),Nil)
//                          ,Nil)
//                      )
//                    )
//                  )
//
//                  shouldParse(
//                    "DNA : DNAComponent()",parser.NestedInstance,
//                    NestedInstance(
//                      InstanceExp(
//                        LocalName("DNA"), ConstructorApp(
//                          TpeConstructor1(
//                            LocalName("DNAComponent"),Nil)
//                          ,Nil)
//                      )
//                    )
//                  )
//
//
//                }
//
//                'InfixConstructorApp - {
//
//                  shouldParse(
//                    "a drives b", parser.InfixConstructorApp,
//                    ConstructorApp(
//                      TpeConstructor1(
//                        LocalName("drives"),Seq(LocalName("a"),LocalName("b"))
//                      ),Nil
//                    )
//                  )
//
//                  shouldNotParse(
//                    "adrivesb", parser.InfixConstructorApp)
//
//                  shouldNotParse(
//                    "1 is_not 2", parser.InfixConstructorApp)
//
//                  shouldNotParse(
//                  "\"a\" drives \"b\"",parser.InfixConstructorApp)
//
//                  shouldParse(
//                  "<SBOL:google> maps_to <www.google.co.uk>",parser.InfixConstructorApp,
//                    ConstructorApp(
//                      TpeConstructor1(
//                        LocalName("maps_to"),Seq(QName(NSPrefix("SBOL"),LocalName("google")),Url("www.google.co.uk"))
//
//                      ),Nil
//                    )
//                  )
//
//                }
//
//                'ConstructorDef - {
//
//                  shouldParse(
//                    "DNAComponent => ComponentDefinition", parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNAComponent"), Nil,
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("ComponentDefinition"), Nil
//
//                        ), Nil
//                      )
//
//                    )
//                  )
//                  shouldParse(
//                    "DNAComponent=>ComponentDefinition", parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNAComponent"), Nil,
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("ComponentDefinition"), Nil
//
//                        ), Nil
//                      )
//
//                    )
//                  )
//
//                  shouldParse(
//                    "DNASequence(x) => Sequence", parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("Sequence"), Nil
//
//                        ), Nil
//                      )
//
//                    )
//                  )
//
//                  shouldParse(
//                    "DNASequence  (x)  =>  Sequence", parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNASequence"), Seq(LocalName("x")),
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("Sequence"), Nil
//
//                        ), Nil
//                      )
//
//                    )
//                  )
//
//                  shouldParse("a => b(x)", parser.ConstructorDef,
//                    ConstructorDef(LocalName("a"), Nil,
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("b"), Seq(LocalName("x"))
//
//                        ), Nil
//                      )
//
//                    )
//                  )
//                  shouldParse(
//                    """DNAComponent => ComponentDefinition
//                      |   type = DNA
//                    """.stripMargin.trim, parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNAComponent"), Nil,
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("ComponentDefinition"), Nil
//
//                        ), Seq(shortbol.Assignment(LocalName("type"), LocalName("DNA")))
//                      )
//
//                    )
//                  )
//
//                  shouldParse(
//                    """DNAComponent => ComponentDefinition
//                      |   type = DNA
//                      |   sequence : DNASequence
//                    """.stripMargin.trim, parser.ConstructorDef,
//                    ConstructorDef(LocalName("DNAComponent"), Nil,
//                      ConstructorApp(
//                        TpeConstructor1(
//                          LocalName("ComponentDefinition"), Nil
//
//                        ), Seq(shortbol.Assignment(LocalName("type"), LocalName("DNA")), NestedInstance(InstanceExp(LocalName("sequence"), ConstructorApp(TpeConstructor1(LocalName("DNASequence"), List()), List()))))
//                      )
//
//                    )
//                  )
//
//
//                }
//                'TopLevels - {
//
//                  quickTopLevelShouldParse(
//                    """
//                    |
//                    |BBa_J61101_seq : DNASequence("aaagacaggacc")
//                    |
//                    |  BBa_J61120_seq : DNASequence("aaagacaggacc")
//                    |    a = b
//                    |    #a equals b
//                    |    a = {helloswldk;lwdw;ldkw;kdw;kdw
//                    |ldkw;lkdw;ldwk;dlwkdwjnkjkajfnwkjnaw}
//                    |
//                    |dna = c
//                    |
//                    |bbc = c""".stripMargin, parser.TopLevels
//                  )
//
//                  quickTopLevelShouldParse(
//                    """
//                      |BBA_J61101_seq : DNASequence("aaaaa")
//                      |  a = b
//                      |  a = b
//                      |
//                      |BBa_J611210_seq : DNASequence("agcaaagc")
//                      |    a = b
//                      |    a = b""".stripMargin,parser.TopLevels
//                  )
//
//                  quickTopLevelShouldParse(
//                    """
//                    |a = b
//                    |
//                    |
//                    |b = a
//                    |b = a
//                    |""".stripMargin,parser.TopLevels
//
//                  )
//
//                  quickTopLevelShouldParse(
//                    """
//                      |seq => Sequence
//                      |  a = b
//                      |  a = b
//                      |
//                      |BBa_J611210_seq : seq""".stripMargin,parser.TopLevels
//
//                  )
//
//                }
//
//    }
//
//}
