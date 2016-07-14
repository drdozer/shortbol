package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML

import scala.concurrent.Future

/**
  *
  *
  * @author Matthew Pocock
  */
case class Introduction() extends TutorialContent {

  override def navigationEntry: View = "Introduction"

  override def render(route: InstantiatedRoute) = {
    import HTML._
    import TutorialUtils._
    Container.Generic(
      Paragraph(v"""
              Welcome to the $shortbol Tutorial.
              $shortbol is a scripting language, designed to be easy to use, powerful and extensible.
              $shortbol is based around structured text to capture your ideas, and doesn't require any prior coding
               skills.
        """),
      Paragraph(v"""
              This tutorial will get you up to speed in how to rapidly prototype synthetic biology designs with $shortbol.
              It works through several steps to introduce the langage, and give you practical experience using it to capture your designs.
              Our running example is a $TetR_gene/$LacI_gene toggle switch (see ${
        Anchor("Gardner 2000")
          .url("http://www.nature.com/nature/journal/v403/n6767/full/403339a0.html")
          .title("Construction of a genetic toggle switch in Escherichia coli")
          .attribute("target", "_blank")
      }), used in our ${
        Anchor("VisBOL")
          .url("http://pubs.acs.org/doi/abs/10.1021/acssynbio.5b00244")
          .title("VisBOL ACS SynBio paper")
          .attribute("target", "_blank")
      } paper."""
      ),
      Paragraph(v"""If you want to skip the tutorial and dive right into bare-metle $shortbol coding, try out the ${
        Anchor("$shortbol Sandbox").url("sandbox.html") } application."""
      )
    )
  }
}
