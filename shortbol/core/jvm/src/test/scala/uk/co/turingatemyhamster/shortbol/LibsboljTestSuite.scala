package uk.co.turingatemyhamster.shortbol

import java.io.{ByteArrayInputStream, StringWriter}
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLOutputFactory

import org.sbolstandard.core2.{SBOLDocument, SBOLFactory}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.shorthandAst.sugar._
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops._
import uk.co.turingatemyhamster.shortbol.ops.rewriteRule.{RepairComponents, RepairIdentities}
import utest._

import scala.util.{Random, Try}
import scalaz._
import Scalaz._

/**
  * Created by nmrp3 on 22/07/16.
  */
object LibsboljTestSuite extends TestSuite {
  val xof = XMLOutputFactory.newInstance()


  val randomIdentifier = {
    val r = new Random()
    val cs = ('A' to 'Z') ++ ('a' to 'z')
    val ds = '0' to '9'
    val as = cs ++ ds

    () => (cs(r.nextInt(cs.size)) :: List.fill(7)(as(r.nextInt(as.size)))).mkString : shorthandAst.Identifier
  }


  def toLibSBOLj(shortbol: String) = {
    val sb = ShortbolParser.SBFile.withPositions("_test_", shortbol).get.value
    val (c, v) = (for {
      e <- sb.eval
      r <- Fixture.doFixup(e)
    } yield r).run(Fixture.configuredContext)

    println("--- scala ---")
    println(v)
    println("--- scala ---")

    val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)

    val rdfIo = RdfIo.rdfIo[datatree.ast.AstDatatree]
    val xmlText = new StringWriter()
    val xtw = xof.createXMLStreamWriter(xmlText)
    RdfIo.write[datatree.ast.AstDatatree](xtw, doc)

    println("--- xml ---")
    println(xmlText)
    println("--- xml ---")

    val sbolDoc = new SBOLDocument()
    SBOLFactory.setSBOLDocument(sbolDoc)
    SBOLFactory.read(new ByteArrayInputStream(xmlText.toString.getBytes(StandardCharsets.UTF_8)))

    sbolDoc
  }


  def fails[F](f: => F): Unit = Try(f).transform(
    r => {
      assert(r != r)
      Try(null)
    },
    e => Try(null)
  )

  override def tests = TestSuite {
    'empty - {
      toLibSBOLj(
        """@import stdlib:sbol
          |
        """.stripMargin)
    }

    'tutorialExamples - {
      'yourFirstScript - {
        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/1#>
              |@defaultPrefix tutorial
              |
              |pTetR : Promoter
              |
              |LacI : CDS
              |""".stripMargin)
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/2#>
              |@defaultPrefix tutorial
              |pTetR : Promoter
              |LacI : CDS
              |""".stripMargin)
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/3#>
              |@defaultPrefix tutorial
              |
              |# Declare a promoter named pTetR
              |pTetR : Promoter
              |
              |# Declare a CDS named LacI
              |LacI : CDS
              |""".stripMargin)
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/4#>
              |@defaultPrefix tutorial
              |
              |pTetR : Promoter
              |  description = "pTet promoter"
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/5#>
              |@defaultPrefix tutorial
              |
              |pTetR : Promoter
              |  # give pTetR a description
              |  description = "pTet promoter"
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/yourFirstScript/6#>
              |@defaultPrefix tutorial
              |
              |pTetR : Promoter
              |  name = "pTetR"
              |  description = "pTetR promoter"
              |  displayId = "BBa_R0040"
            """.stripMargin
          )
        }
      }

      'types - {
        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/types/1#>
              |@defaultPrefix tutorial
              |
              |# Declare an instance named pTetR, of type Promoter
              |pTetR : Promoter
              |  name = "pTetR"
              |  description = "pTetR promoter"
              |  displayId = "BBa_R0040"
              |
              |# Declare an instance named LacI, of type CDS
              |LacI : CDS
              |  name = "LacI"
              |  description = "LacI protein coding region"
              |  displayId = "P03023"
            """.stripMargin
          )
        }
      }

      'addingSequences - {
        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/1#>
              |@defaultPrefix tutorial
              |
              |lacITSeq : DnaSequence("ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa")
            """.stripMargin
          )
        }

        * - {
          fails(
            toLibSBOLj(
              """@import stdlib:sbol
                |
                |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/2#>
                |@defaultPrefix tutorial
                |
                |lacITSeq : DnaSequence({
                |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
                |  aggatcggcg gttttctttt ctcttctcaa
                |  })
              """.stripMargin
            )
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/3#>
              |@defaultPrefix tutorial
              |
              |lacITSeq : DnaSequence({
              |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
              |  aggatcggcg gttttctttt ctcttctcaa
              |  }^^edam:fasta)
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/4#>
              |@defaultPrefix tutorial
              |
              |lacITSeq : DnaSequence({
              |        1 ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
              |       61 aggatcggcg gttttctttt ctcttctcaa
              |}^^edam:genbank)
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/5#>
              |@defaultPrefix tutorial
              |
              |lacIT : Terminator
              |  sequence = lacItSeq
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/6#>
              |@defaultPrefix tutorial
              |
              |lacITSeq : DnaSequence({
              |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
              |  aggatcggcg gttttctttt ctcttctcaa
              |  }^^edam:fasta)
              |
              |lacIT : Terminator
              |  sequence = lacItSeq
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/addingSequences/7#>
              |@defaultPrefix tutorial
              |
              |pTetR : Promoter
              |    sequence : DnaSequence("tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac")
            """.stripMargin
          )
        }
      }

      'composingDesigns - {

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDesigns/1#>
              |@defaultPrefix tutorial
              |
              |# The genetic parts of the TetR inverter
              |pTetR   : Promoter
              |lacIRbs : RBS
              |LacI    : CDS
              |lacIT   : Terminator
              |
              |# The composite device for the TetR inverter
              |tetRInverter : DnaComponent
              |  component = pTetR
              |  component = lacIRbs
              |  component = LacI
              |  component = lacIT
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDesigns/2#>
              |@defaultPrefix tutorial
              |
              |# The composite device for the TetR inverter
              |tetRInverter : DnaComponent
              |  # include the child components
              |  component = pTetR
              |  component = lacIRbs
              |  component = LacI
              |  component = lacIT
              |  # relative positions of child components
              |  sequenceConstraint = pTetR   precedes lacIRbs
              |  sequenceConstraint = lacIRbs precedes LacI
              |  sequenceConstraint = LacI    precedes lacIT
              |
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDesigns/3#>
              |@defaultPrefix tutorial
              |
              |# The composite device for the TetR inverter
              |tetRInverter : DnaComponent
              |  # relative positions of child components
              |  sequenceConstraint = pTetR   precedes lacIRbs
              |  sequenceConstraint = lacIRbs precedes LacI
              |  sequenceConstraint = LacI    precedes lacIT
              |
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDesigns/4#>
              |@defaultPrefix tutorial
              |
              |dc : DnaComponent
              |  # using infix notation
              |  sequenceConstraint = promoter precedes open_reading_frame
              |  # the same thing, but using a constructor
              |  sequenceConstraint : precedes(promoter, open_reading_frame)
            """.stripMargin
          )
        }

        * - {
          toLibSBOLj(
            """@import stdlib:sbol
              |
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDesigns/5#>
              |@defaultPrefix tutorial
              |
              |# The composite device for the TetR inverter
              |tetRInverter : DnaComponent
              |  # absolute positions of child components
              |  sequenceAnnotation : at(pTetR,      1,   55, inline)
              |  sequenceAnnotation : at(lacIRbs,   56,   68, inline)
              |  sequenceAnnotation : at(LacI,      69, 1197, inline)
              |  sequenceAnnotation : at(lacIT,   1198, 1288, inline)
            """.stripMargin
          )
        }
      }
    }

    'others - {
      'subtlinReceiver - {
        toLibSBOLj(
          """@import stdlib:sbol
            |
            |@prefix test <http://me.name/test#>
            |@prefix psimi <http://identifiers.org/psimi/MI:>
            |@prefix ico2s <http://ico2s.org/subtlinExample#>
            |@defaultPrefix ico2s
            |
            |Phosphorylation = <http://identifiers.org/biomodels.sbo/SBO:0000216>
            |Acceptor   = <http://identifiers.org/psimi/MI:0843>
            |Donor      = <http://identifiers.org/psimi/MI:0842>
            |Modifier   = <http://identifiers.org/biomodels.sbo/SBO:0000019>
            |Product    = <http://identifiers.org/biomodels.sbo/SBO:0000011>
            |GeneticProduction   = <http://identifiers.org/biomodels.sbo/SBO:0000589>
            |
            |phosphorylates(sbj, obj) => sbol:Interaction
            |  sbol:type = Phosphorylation
            |  sbol:participation : sbol:Participation
            |      sbol:participant = sbj
            |      sbol:role = Donor
            |  sbol:participation : sbol:Participation
            |      sbol:participant = obj
            |      sbol:role = Acceptor
            |
            |encodes(sbj, obj) => sbol:Interaction
            |  sbol:type = GeneticProduction
            |  sbol:participation : sbol:Participation
            |      sbol:participant = sbj
            |      sbol:role = Modifier
            |  sbol:participation : sbol:Participation
            |      sbol:participant = obj
            |      sbol:role = Product
            |
            |SubtilinReceiver : DnaComponent
            |    component= pspark
            |    component = rbs1
            |    component = rbs1
            |    component = spaR
            |    component = rb2
            |    component = spaK
            |    component = ter1
            |    component = pspas
            |    component = rbs3
            |    component = gfp
            |    component = ter2
            |
            |pspark : Promoter
            |  displayId = "pspark"
            |  name = "PspaRK"
            |  description = "Constitutive promoter"
            |
            |rbs1 : RBS
            |spaK : CDS
            |rbs2 : RBS
            |spaR : CDS
            |ter1 : Terminator
            |
            |pspas : Promoter
            |rbs3 : RBS
            |gfp : CDS
            |ter2 : Terminator
            |
            |SpaKP : ProteinComponent
            |SpaRP : ProteinComponent
            |GFPP : ProteinComponent
            |
            |Subtilin: SmallMoleculeComponent
            |
            |SubtilinReceiverModule : ModuleDefinition
            |   interaction = SpaKP phosphorylates SpaRP
            |   interaction = Subtilin phosphorylates SpaKP
            |   interaction = spaK encodes SpaKP
            |   interaction = spaR encodes SpaRP
            |   interaction = gfp encodes GFPP""".stripMargin
        )
      }
    }
  }
}
