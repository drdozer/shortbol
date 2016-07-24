package uk.co.turingatemyhamster.shortbol

import java.io.{ByteArrayInputStream, StringWriter}
import java.nio.charset.StandardCharsets
import javax.xml.stream.XMLOutputFactory

import org.sbolstandard.core2.{SBOLDocument, SBOLFactory}
import uk.co.turingatemyhamster.datatree
import uk.co.turingatemyhamster.datatree.io.RdfIo
import uk.co.turingatemyhamster.shortbol.ast.sugar._
import uk.co.turingatemyhamster.shortbol.ops.Eval.EvalOps
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser.POps
import uk.co.turingatemyhamster.shortbol.ops._
import utest._

import scala.util.Try
import scalaz._

/**
  * Created by nmrp3 on 22/07/16.
  */
object LibsboljTestSuite extends TestSuite {
  val xof = XMLOutputFactory.newInstance()

  def toLibSBOLj(shortbol: String) = {
    val sb = ShortbolParser.SBFile.withPositions("_test_", shortbol).get.value
    val (c, v) = sb.eval.run(Fixture.configuredContext)

    val tc = OWL((c, v))
    if(tc.isFailure) {
      val errs = tc.leftMap(_ map (_.prettyPrint))
      assert(errs.isSuccess)
    } else {
      val doc = Exporter[datatree.ast.AstDatatree](c).apply(v)

      val rdfIo = RdfIo.rdfIo[datatree.ast.AstDatatree]
      val xmlText = new StringWriter()
      val xtw = xof.createXMLStreamWriter(xmlText)
      RdfIo.write[datatree.ast.AstDatatree](xtw, doc)

      val sbolDoc = new SBOLDocument()
      SBOLFactory.setSBOLDocument(sbolDoc)
      SBOLFactory.read(new ByteArrayInputStream(xmlText.toString.getBytes(StandardCharsets.UTF_8)))

      sbolDoc
    }
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
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDeigns/1#>
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
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDeigns/2#>
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
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDeigns/3#>
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
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDeigns/4#>
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
              |@prefix tutorial <http://shortbol.ico2s.org/tutorial/composingDeigns/5#>
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
  }
}
