package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML

import scala.concurrent.Future

/**
  *
  *
  * @author Matthew Pocock
  */
case class TutorialRoot() extends TutorialContent {

  override def navigationEntry: View = "Introduction"

  override def render(route: InstantiatedRoute) = {
    import HTML._
    Container.Generic(
      Paragraph(
        """Welcome to the Shortbol Tutorial.
          |Shortbol is a scripting language, designed to be easy to use, powerful and extensible.
          |Shortbol is based around structured text to capture your ideas, and doesn't require any coding skills.
          |""".stripMargin),
      Paragraph(
        """This tutorial will get you up to speed in how to rapidly prototype synthetic biology designs with Shortbol.
          |It works through several steps to introduce the langage, and give you practical experience using it to capture your designs.
          |Our running example is a TetR/LacI toggle switch, described in our
          |""".stripMargin,
        Anchor("VisBOL").url("http://pubs.acs.org/doi/abs/10.1021/acssynbio.5b00244").title("VisBOL ACS SynBio paper").attribute("target", "_blank"),
        " paper."
      ),
      Paragraph(
        "If you want to skip the tutorial and dive right into bare-metle Shortbol coding, try out the ",
        Anchor("Shortbol Sandbox").url("sandbox.html"),
        " application."
      )
    )
  }
}
