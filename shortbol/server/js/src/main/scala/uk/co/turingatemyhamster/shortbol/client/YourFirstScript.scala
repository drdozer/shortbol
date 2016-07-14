package uk.co.turingatemyhamster.shortbol.client

import fastparse.core.Parsed.Success
import org.widok._
import org.widok.bindings.HTML
import pl.metastack.metarx.ReadChannel
import uk.co.turingatemyhamster.shortbol.ast._
import sugar._

import scala.concurrent.Future
import scala.scalajs.js

/**
  *
  *
  * @author Matthew Pocock
  */
case class YourFirstScript() extends TutorialContent {
  import TutorialUtils._

  override def navigationEntry: View = "Your First Script"

  override def render(route: InstantiatedRoute) = {
    import HTML._
    Container.Generic(
      Section(
        Heading.Level2("Your first script"),
        Paragraph(v"""
              We're going to start by building the $shortbol for the $TetR_gene inverter of the $TetR_gene/$LacI_gene toggle switch.
          """),
        Paragraph(v"""
              The $TetR_gene inverter couples a tetracycline-repressed promoter with the $LacI_gene coding sequence, so that in
               the absence of tetracycline, $LacI_gene is produced.
              We are going to describe the design of the $TetR_gene inverter using $shortbol.
          """),
        AceEditor(
          """@import <stdlib:sbol>
            |
            |pTetR : Promoter
            |
            |LacI : CDS""".stripMargin).width(Length.Percentage(80)).height(Length.Pixel(80)).isReadOnly(true),
        Paragraph(v"""
              This script contains three lines.
              The first line imports the $sbol standard library.
              You will usually have this at the top of all your $shortbol scripts.
              The second line declares a promoter called $pTetR, and the third declares a complement
              determining region (open reading frame, or coding region) called $LacI."""),
        Paragraph(v"""
              In this example we have separated each line with a blank line.
              Blank lines are ignored by $shortbol, but can make reading a script easier.
              This script, without the newlines means exactly the same thing.
          """),
        AceEditor(
          """@import <stdlib:sbol>
            |pTetR : Promoter
            |LacI : CDS""".stripMargin).width(Length.Percentage(80)).height(Length.Pixel(60)).isReadOnly(true),
        Paragraph(
          v"""I find that more difficult to read, but it's down to personal taste."""
        ),
        Paragraph(v"""
              Comments an be added to the script.
              Any line starting with a pound '${code("#")}' character is treated as a comment, and ignored.
              """),
        AceEditor(
          """# Import the SBOL standard library
            |@import <stdlib:sbol>
            |
            |# Declare a promoter named pTetR
            |pTetR : Promoter
            |
            |# Declare a CDS named LacI
            |LacI : CDS""".stripMargin).width(Length.Percentage(80)).height(Length.Pixel(100)).isReadOnly(true)
      ),
      Section(
        Heading.Level2("Adding some properties"),
        Paragraph(v"""
              So far we have created a promoter and a CDS and named them.
              With $shortbol we can attach properties and values to these instances.
              $shortbol uses indentation to make lines 'part of' a containing instance.
          """),
        Paragraph(
          v"""We can add a description to $pTetR like this:"""),
        AceEditor(
          """pTetR : Promoter
            |  description = "pTet promoter"
          """.stripMargin
        ).width(Length.Percentage(80)).height(Length.Pixel(40)).isReadOnly(true),
        Paragraph(v"""
              You can add any names and values you like to an instance, and these become extra annotating data.
              However, some of these names mean something special to $sbol.
              For example, $displayId is a property that captures an identifier that can be displayed on a diagram or
               parts listing.
              In our $TetR_gene inverter example, the $pTetR part actually comes from the biobrick ${code("BBa_R0040")},
               so this is a good candidate for a $displayId value.
          """),
        AceEditor(
          """pTetR : Promoter
            |  description = "pTet promoter"
            |  displayId = "BBa_R0040"
          """.stripMargin
        ).width(Length.Percentage(80)).height(Length.Pixel(60)).isReadOnly(true)
      ),
      Section(
        Heading.Level2("Your turn"),
        Paragraph(v"""
              Now it is your turn.
              So far all the code examples have been read-only.
              You can edit the next sample, like any text area.
          """),
        Paragraph(v"""
              Start with the provided skeleton script and modify it so that $LacI has the $description
                "LacI protein coding region" and $displayId of "P03023".
              Don't forget to indent the property names.
          """),
        AceEditor(
          """LacI : CDS
            |""".stripMargin
        ).width(Length.Percentage(40))
          .height(Length.Pixel(60))
          .isReadOnly(false)
          .rememberAs(yourTurn = _),
        Container.Generic(
          yourTurn.checkList(
            yourTurn.check(
              v"""set the $description to "LacI protein coding region"""", "LacI", "description" -> slLit("LacI protein coding region")),
            yourTurn.check(
              v"""set the $displayId to "P03023"""", "LacI", "displayId" -> slLit("P03023"))
          )
        ).width(Length.Percentage(40))
      )
    )
  }

  var yourTurn: AceEditor = _
}




