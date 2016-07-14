package uk.co.turingatemyhamster.shortbol.client
import org.widok._
import org.widok.bindings.HTML._
import TutorialUtils._
import uk.co.turingatemyhamster.shortbol.ast.sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
case class AddingSequences() extends TutorialContent {
  override def navigationEntry: View = "Adding Sequences"

  override def render(route: InstantiatedRoute) = Container.Generic(
    Section(
      Heading.Level2("Adding Sequences"),
      Paragraph(v"""
              Ultimately, the units that are assembled for a genome design are DNA sequences.
              $shortbol has a type called $DnaSequence that lets you specify a DNA sequence.
        """),
      AceEditor(
        """lacITSeq : DnaSequence("ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa")"""
      ).width(Length.Percentage(40))
        .height(Length.Pixel(30))
        .isReadOnly(true),
      Paragraph(v"""
              Here we have constructed a $DnaSequence named $lacITSeq, and rather than setting a property, the DNA sequence
               string is passed into the $DnaSequence constructor.
              $shortbol instances are often created by giving the type constructor some values to work with.
              The constructor will use these to set up properties for you.
        """),
      Paragraph(v"""
              Until now, we have been using inline strings with values protected with quote '${code("\"")}' marks.
              These are convenient for short stretches of text, but quickly become unweildy for large blocks of text.
              $shortbol supports multi-line quotes to let you spread a long block of text over many lines.
              We could have written the previous sequence instance like this:
        """),
      AceEditor(
        """lacITSeq : DnaSequence({
          |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
          |  aggatcggcg gttttctttt ctcttctcaa
          |  })
        """.stripMargin
      ).width(Length.Percentage(40))
              .height(Length.Pixel(60))
              .isReadOnly(true),
      Paragraph(v"""
             The multi-line quote is started with an opening brace '{' followed by an indented block of text, and then a
              closing '}'.
             You can use any amount of indent that makes the text easy to read and edit.
             $shortbol works out what the indent is by looking at how indented the closing '}' is.
                """)
    ),
    Section(
      Heading.Level2("Attaching the sequence to the terminator"),
      Paragraph(v"""
              Now that we know how to make a sequence, we need to attach it to the corresponing part.
              This is done in the same way that we set the description and displayId for the parts earlier.
              $sbol defines a property called sequence that links from a part back to the sequence it has.
              This time, rather than quoting the value, we use the naked value.
              This tells $shortbol that we are linking to another instance, rather than quoting a value.
        """),
      AceEditor(
        """lacIT : Terminator
          |  sequence = lacItSeq
        """.stripMargin
      ).width(Length.Percentage(40))
                    .height(Length.Pixel(60))
                    .isReadOnly(true),
      Paragraph(
        v"""Putting it all together, we have this $shortbol script:"""
      ),
      AceEditor(
        """@import <stdlib:sbol>
          |
          |lacITSeq : DnaSequence({
          |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
          |  aggatcggcg gttttctttt ctcttctcaa
          |  })
          |
          |lacIT : Terminator
          |  sequence = lacItSeq
        """.stripMargin
      ).width(Length.Percentage(40))
        .height(Length.Pixel(120))
        .isReadOnly(true)
    ),
    Section(
      Heading.Level2("Your Turn"),
      Paragraph(
        v"""It's your turn to create a sequence for the $pTetR_gene promoter and attach it to the promoter."""
      ),
      AceEditor("")
        .width(Length.Percentage(40))
        .height(Length.Pixel(60))
        .isReadOnly(false)
        .rememberAs(yourTurn = _),
      yourTurn.checkList(
        yourTurn.check(v"Create a $Promoter called $pTetR", "pTetR", "Promoter"),
        yourTurn.check(v"Create a $DnaSequence called $pTetRSeq with the DNA sequence tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac (you may want to cut and paste)", "pTetRSeq", "DnaSequence"),
        yourTurn.check(v"Set the $sequence property of $pTetR equal to $pTetRSeq", "pTetR", "sequence" -> "pTetRSeq")
      )
    ),
    Section(
      Heading.Level2("Scruffy sequences"),
      Paragraph(v"""
              Usually you will want to name your sequences separately from the components that use them.
              This allows the same sequence to be annotated independently, without needing to dublicate the DNA string.
              However, $shortbol is designed with quick prototyping of designs in mind, so it lets you be scruffy.
              It actually allows you to create the DNA sequence in-place as the value of the sequence property.
              Under the hood, this creates a new $DnaSequence instance with a randomised name, and sets that
               as the value of the $sequence property.
              It isn't best practice, but when deadlines loom, or when you are going to throw away the script anyway,
               well, who's watching?
        """),
      AceEditor(
        """pTetR : Promoter
          |    sequence : DnaSequence("tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac")
        """.stripMargin
      )
        .width(Length.Percentage(40))
        .height(Length.Pixel(60))
        .isReadOnly(true),
      Paragraph(v"""
              The main difference here is that instead of assigning a value to the $sequence property, we are calling a
               type constructor.
              So instead of using '${code("=")}' to assign a value, we use '${code(":")}' to construct one.
              This pattern is used in a number of places in $shortbol scripts to build up nested instances within instances.
        """)
    )
  )

  var yourTurn: AceEditor = _
}
