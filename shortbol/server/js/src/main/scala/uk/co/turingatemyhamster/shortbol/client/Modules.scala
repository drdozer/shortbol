package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML._
import TutorialUtils._

/**
  *
  *
  * @author Matthew Pocock
  */
case class Modules() extends TutorialContent {
  override def navigationEntry = "Modules"

  override def render(route: InstantiatedRoute) = Container.Generic(
    Section(
      Heading.Level2("Modules"),
      Paragraph(
        v"""
            Up until now we have been building descriptions of the physical design, by describing the stuff that makes
             it up.
            Usually, the physical parts are artifacts of achieving a desired ${emph("behaviour")}, rather than being an
             ends in their own right.
            The $sbol data standard provides a rich, compositional model for describing the intended behaviour of a
             design, in parallel to the desired structure.
            This is captured by the $ModuleDefinition type.
          """),
      Paragraph(
        v"""
            The $ModuleDefinition data model groups together the participating physical parts, their interactions, links
             to numerical models if they exist, and sub-modules.
            Here we will cover the physical parts and their interactions.
          """
      )
    ),
    Section(
      Heading.Level2("A Module"),
      Paragraph(
        v"""
          In the functional design of the TetR inverter, the $TetR_protein protein represses the expression of the
           $LacI_protein.
          To capture this, we first need to create a module, and add components for $TetR_protein and $LacI_protein to
           it.
           Then we add an interaction to say that $TetR_protein represses $LacI_protein.
          """),
      aceExample(
        """@import stdlib:sbol
          |@prefix tutorial <http://shortbol.ico2s.org/tutorial/modules/1#>
          |@defaultPrefix tutorial
          |
          |# The TetR and LacI proteins
          |TetR : ProteinComponent
          |LacI : ProteinComponent
          |
          |# The TetR inverter
          |TetR_inverter : ModuleDefinition
          |  description = "TetR inverter."
          |  functionalComponent = TetR
          |  functionalComponent = LacI
          |  interaction = TetR represses LacI
        """.stripMargin
      )
    )
  )
}
