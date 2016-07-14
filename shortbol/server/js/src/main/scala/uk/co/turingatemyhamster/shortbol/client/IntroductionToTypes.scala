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
        Paragraph(v"""
              In the previous tutorial, we created instances to represent $pTetR_gene and $LacI_gene in the $TetR_gene
               inverter device, and gave them ${description}s and ${displayId}s.
              When we put it all together, that example looks like this:
          """),
        AceEditor(
          """@import <stdlib:sbol>
            |
            |pTetR : Promoter
            |  description = "pTet promoter"
            |  displayId = "BBa_R0040"
            |
            |LacI : CDS
            |  description = "LacI protein coding region"
            |  displayId = "P03023"
          """.stripMargin
        ).width(Length.Percentage(40))
          .height(Length.Pixel(130))
          .isReadOnly(true),
        Paragraph(v"""
              Let's look at this example again.
              It starts with an import of the $sbol standard library.
              Then it declares two instances, a $Promoter called $pTetR and a $CDS called $LacI.
              The $Promoter and $CDS are types.
              They say what sort of thing $pTetR and $LacI are.
              In $shortbol, whenever you declare an instance, you construct it with a type.
              The name of the instance and the name of the type are separeted by a colon '${code(":")}'
               surrounded by spaces.
                  """),
        Paragraph(v"""
              The $sbol standard library that we imported provides a pallet of types that can be used in your designs.
              There are type constructors for all the common types of genetic parts.
              Here are some of the ones you may use most frequently:
          """),
        List.Unordered(
          List.Item(
            Container.Inline(Promoter).css("name"), ": ",
            Container.Inline("A genomic region where transcription is initiated.").css("description")),
          List.Item(
            Container.Inline(CDS).css("name"), ": ",
            Container.Inline("A complement determining sequence; a genomic region that encodes a protein.").css("description")),
          List.Item(
            Container.Inline(Terminator).css("name"), ": ",
            Container.Inline("A genomic region that terminates transcription.").css("description")),
          List.Item(
            Container.Inline(RBS).css("name"), ": ",
            Container.Inline("A ribosome binding region, where the ribosome will bind to a transcript.").css("description")),
          List.Item(
            Container.Inline(Operator).css("name"), ": ",
            Container.Inline("A region where proteins bind to regulate transcription.").css("description"))
        ),
        Paragraph(
          """You can add any number of these genetic parts to your design.
            |Just give them each a unique name within your script.
          """.stripMargin
        )
      ),
      Section(
        Heading.Level2("Your Turn"),
        Paragraph(v"""
              The $TetR_gene inverter is made of four parts. A promoter, RBS, CDS and terminator.
              In the editor below, create the corresponding instances.
          """),
        AceEditor("")
          .width(Length.Percentage(40))
          .height(Length.Pixel(120))
          .isReadOnly(false)
          .rememberAs(allParts = _),
        allParts.checkList(
          allParts.check(v"a $Promoter instance called $pTetR", "pTetR", "Promoter"),
          allParts.check(v"an $RBS instance called $lacIRbs", "lacIRbs", "RBS"),
          allParts.check(v"a $CDS instance called $LacI", "LacI", "CDS"),
          allParts.check(v"a $Terminator instance called $lacIT", "lacIT", "Terminator")
        )
      )
    )
  }

  var allParts: AceEditor = _
}
