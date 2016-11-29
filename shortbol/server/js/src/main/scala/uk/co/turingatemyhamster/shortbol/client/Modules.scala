package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML._
import TutorialUtils._
import uk.co.turingatemyhamster.shortbol.shorthandAst._
import uk.co.turingatemyhamster.shortbol.shorthandAst.sugar._

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
              |  description = "TetR inverter"
              |  functionalComponent = TetR
              |  functionalComponent = LacI
              |  interaction = TetR represses LacI
              |""".stripMargin
          ),
      Paragraph(
        v"""
          The LacI inverter is very similar, but in this module $LacI_protein represses $TetR_protein.
          Complete the script below to include this interaction.
        """
      ),
      aceTask(6,
        """LacI_inverter : ModuleDefinition
          |  description = "LacI inverter"
          |""".stripMargin,
        check(v"Add a $functionalComponent value for $TetR_protein", "LacI_inverter", "functionalComponent" := "TetR"),
        check(v"Add a $functionalComponent value for $LacI_protein", "LacI_inverter", "functionalComponent" := "LacI"),
        check(v"Add a $represses interaction between $LacI_protein and $TetR_protein", "LacI_inverter", "interaction", "represses", "LacI"::("TetR" : ValueExp)::Nil)
      )
    ),
    Section(
      Heading.Level2("Composing modules"),
      Paragraph(
        v"""
          In the previous section, we build two modules, one for TetR inverter and one for the LacI inverter.
          The next step is to combine these into a toggle-switch module.
          To do this, we create a new $ModuleDefinition that imports the two inverters.
          """
      ),
      aceExample(
        """ToggleSwitch : ModuleDefinition
          |  description = "LacI/TetR toggle switch"
          |  module = TetR_Inverter
          |  module = LacI_Inverter
        """.stripMargin
      ),
      Paragraph(
        v"""
           This composite module contains all of the behaviour of both the TetR and LacI inverter modules.
           However, at the moment both of the inverters are 'black box', with completely independent behaviour.
           What we want to do is glue them together, so that they are using the same pool of $TetR_protein and
           $LacI_protein molecules.
           This will cause them to repress one-another, flip-flopping between repressing $TetR_protein levels and
           $LacI_protein levels.
         """),
      Paragraph(
        v"""
          To achieve this, we need to wire components in the sub-modules.
          This is done using the $mapsTo property.
          We create placeholder components in the super-module, and then use $mapsTo to wire components in the
           sub-modules to that component.
          By wiring $TetR_protein from both inverters to the same component in the super-module, we identify them with
           a shared molecule pool.
          This couples the behaviour of the two inverters, so that one now affects the levels of molecues used by the
           other.
          The other change in this example is that because the $mapsTo property is defined on the
           $Module, we have to create the $Module instance the long way, with an explicit $definition,
           rather than relying upon $shortbol to generate one for us given a reference.
          """
      ),
      aceExample(
        """ToggleSwitch : ModuleDefinition
          |  description = "LacI/TetR toggle switch"
          |  functionalComponent = OurLacI
          |  functionalComponent = OurTetR
          |  module : Module
          |    definition = TetR_Inverter
          |    mapsTo = OurLacI mergedWith LacI
          |    mapsTo = OurTetR mergedWith TetR
          |  module : Module
          |    definition = LacI_Inverter
          |    mapsTo = OurLacI mergedWith LacI
          |    mapsTo = OurTetR mergedWith TetR
        """.stripMargin
      )
    )
  )
}
