package uk.co.turingatemyhamster.shortbol
package client

import org.widok._

import ast.sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
case class IntroductionToTypes() extends TutorialContent {
  import TutorialUtils._

  override def navigationEntry: View = "Introduction to Types"

  override def render(route: InstantiatedRoute) = {
    import org.widok.bindings.HTML._

    Container.Generic(
      Section(
        Heading.Level2("Types"),
        Paragraph(
          """In the previous tutorial, we created instances to represent pTetR and LacI in the TetR inverter device,
            | and gave them descriptions and displayIds.
            |When we put it all together, that example looks like this:
          """.stripMargin
        ),
        AceEditor(
          """@import <stdlib:sbol>
            |
            |pTetR : Promoter
            |  description = "pTet promoter"
            |  displayId = "BBa_R0040"
            |
            |lacI : CDS
            |  description = "LacI protein"
            |  displayId = "P03023"
          """.stripMargin
        ).width(Length.Percentage(40))
          .height(Length.Pixel(130))
          .isReadOnly(true),
        Paragraph(
          """Let's look at this example again.
            |It starts with an import of the sbol standard library.
            |Then it declares two instances, a Promoter called pTetR and a CDS called lacI.
            |The Promoter and CDS are types.
            |They say what sort of thing pTetR and lacI are.
            |In shortbol, whenever you declare an instance, you construct it with a type.
          """.stripMargin),
        Paragraph(
          """The SBOL standard library that we imported provides a pallet of types that can be used in your designs.
            |There are constructors for all the common types of genetic parts. These include:
          """.stripMargin
        ),
        List.Unordered(
          List.Item(
            Container.Inline("Promoter").css("name"), ": ",
            Container.Inline("A genomic region where transcription is initiated.").css("description")),
          List.Item(
            Container.Inline("CDS").css("name"), ": ",
            Container.Inline("A complement determining sequence; a genomic region that encodes a protein.").css("description")),
          List.Item(
            Container.Inline("Terminator").css("name"), ": ",
            Container.Inline("A genomic region that terminates transcription.").css("description")),
          List.Item(
            Container.Inline("RBS").css("name"), ": ",
            Container.Inline("A ribosome binding region, where the ribosome will bind to a transcript.").css("description")),
          List.Item("Operator")
        ),
        Paragraph(
          """You can add any number of these genetic parts to your design.
            |Just give them each a unique name within your script.
          """.stripMargin
        )
      ),
      Section(
        Heading.Level2("Your Turn"),
        Paragraph(
          """The TetR inverter is made of four parts. A promoter, RBS, CDS and terminator.
            |In the editor below, create the corresponding instances.""".stripMargin),
        AceEditor("")
          .width(Length.Percentage(40))
          .height(Length.Pixel(120))
          .isReadOnly(false)
          .rememberAs(allParts = _),
        allParts.checkList(
          allParts.check("a Promoter instance called pTetR", "pTetR", "Promoter"),
          allParts.check("an RBS instance called lacIRbs", "lacIRbs", "RBS"),
          allParts.check("a CDS instance called lacI", "lacI", "CDS"),
          allParts.check("a terminator instance called lacIT", "lacIT", "Terminator")
        )
      )
    )
  }

  var allParts: AceEditor = _
}
