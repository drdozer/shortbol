package uk.co.turingatemyhamster.shortbol

import ops._
import Eval.EvalOps
import ShortbolParser.POps
import ast.sugar._
import uk.co.turingatemyhamster.shortbol.ast.SBEvaluatedFile
import utest._

/**
  *
  *
  * @author Matthew Pocock
  */
object SbolConstraintTestSuite extends TestSuite {

  val sbol = ConstraintSystem(SBOL)

  def succeeds(src: String): Unit = {
    val (c, v) = ShortbolParser.SBFile.withPositions("_test_", src).get.value.eval.run(Fixture.configuredContext)
    sbol()(c)(v).fold(
      nel => assert(nel == null),
      success => ()
    )
  }

  def fails(src: String): Unit = {
    val (c, v) = ShortbolParser.SBFile.withPositions("_test_", src).get.value.eval.run(Fixture.configuredContext)
    sbol()(c)(v).fold(
      nel => {
      },
      success => assert(false)
    )
  }

  override def tests = TestSuite {
    'topLevel - {

      'nonTopLevel - {
        'noAbout - {
          succeeds(
            """
              |x : foaf:Person
              |  foaf:givenName = "bob"
            """.stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : foaf:Person
              |  rdf:about = <http://foo.bar.com/bob>
            """.stripMargin
          )
        }
      }

      'topLevel - {
        'noAbout - {
          succeeds(
            """
              |x : sbol:TopLevel
              |  foaf:givenName = "bob"
            """.stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : sbol:TopLevel
              |  rdf:about = <http://foo.bar.com/bob>
            """.stripMargin
          )
        }
      }
    }

    'nestedNonIdentified - {

      'nonTopLevel - {
        'noAbout - {
          succeeds(
            """
              |x : prov:Agent
              |  prov:actedOnBehalfOf : foaf:Person
              |    foaf:givenName = "bob"
            """.stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : prov:Agent
              |  prov:actedOnBehalfOf : foaf:Person
              |    rdf:about = <http://foo.bar.com/bob>
            """.stripMargin
          )
        }
      }

      'topLevel - {
        'noAbout - {
          succeeds(
            """
              |x : sbol:TopLevel
              |  prov:actedOnBehalfOf : foaf:Person
              |    foaf:givenName = "bob"
              |""".stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : sbol:TopLevel
              |  prov:actedOnBehalfOf : foaf:Person
              |    rdf:about = <http://foo.bar.com/bob>
              |""".stripMargin
          )
        }
      }
    }

    'nestedIdentified - {

      'nonTopLevel - {
        'noAbout - {
          succeeds(
            """
              |x : prov:Agent
              |  prov:actedOnBehalfOf : sbol:Identified
              |    foaf:givenName = "bob"
            """.stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : prov:Agent
              |  prov:actedOnBehalfOf : sbol:Identified
              |    rdf:about = <http://foo.bar.com/bob>
            """.stripMargin
          )
        }
      }

      'topLevel - {
        'noAbout - {
          fails(
            """
              |x : sbol:TopLevel
              |  prov:actedOnBehalfOf : sbol:Identified
              |    foaf:givenName = "bob"
              |""".stripMargin
          )
        }

        'about - {
          succeeds(
            """
              |x : sbol:TopLevel
              |  prov:actedOnBehalfOf : sbol:Identified
              |    rdf:about = <http://foo.bar.com/bob>
              |""".stripMargin
          )
        }
      }
    }


  }

}
