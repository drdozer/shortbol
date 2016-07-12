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
      Paragraph(
        """Ultimately, the units that are assembled for a genome design are DNA sequences.
          |Shortbol has a type called DnaSequence that lets you specify a DNA sequence.
        """.stripMargin
      ),
      AceEditor(
        """lacITSeq : DnaSequence("ttcagccaaaaaacttaagaccgccggtcttgtccactaccttgcagtaatgcggtggacaggatcggcggttttcttttctcttctcaa")"""
      ).width(Length.Percentage(40))
        .height(Length.Pixel(30))
        .isReadOnly(true),
      Paragraph(
        """Here we have constructed a DnaSequence named lacITSeq, and rather than setting a property, the DNA sequence
          | string is passed into the DnaSequence constructor.
          |Shortbol instances are often created by giving the type constructor some values to work with.
          |The constructor will use these to set up properties for you.
          |""".stripMargin
      ),
      Paragraph(
        """Until now, we have been using inline strings, values protected with quote marks.
          |These are convenient for short stretches of text, but quickly become unweildy for large blocks of text.
          |Shortbol supports multi-line quotes to let you spread a long block of text over many lines.
          |We could have written the previous sequence instance like this:
        """.stripMargin
      ),
      AceEditor(
        """lacITSeq : DnaSequence({
          |  ttcagccaaa aaacttaaga ccgccggtct tgtccactac cttgcagtaa tgcggtggac
          |  aggatcggcg gttttctttt ctcttctcaa
          |  })
        """.stripMargin
      ).width(Length.Percentage(40))
              .height(Length.Pixel(60))
              .isReadOnly(true),
      Paragraph(
        """The multi-line quote is started with an opening brace '{' followed by an indented block of text, and then a
          | closing '}'.
          |You can use any amount of indent that makes the text easy to read and edit.
          |Shortbol works out what the indent is by looking at how indented the closing '}' is.
        """.stripMargin
      )
    ),
    Section(
      Heading.Level2("Attaching the sequence to the terminator"),
      Paragraph(
        """Now that we know how to make a sequence, we need to attach it to the corresponing part.
          |This is done in the same way that we set the description and displayId for the parts earlier.
          |SBOL defines a property called sequence that links from a part back to the sequence it has.
          |This time, rather than quoting the value, we use the naked value.
          |This tells shortbol that we are linking to another instance, rather than
        """.stripMargin),
      AceEditor(
        """lacIT : Terminator
          |  sequence = lacItSeq
        """.stripMargin
      ).width(Length.Percentage(40))
                    .height(Length.Pixel(60))
                    .isReadOnly(true),
      Paragraph(
        """Putting it all together, we have this shortbol script:"""
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
        """It's your turn to create a sequence for the pTetR promoter and attach it to the promoter."""
      ),
      AceEditor("")
        .width(Length.Percentage(40))
        .height(Length.Pixel(60))
        .isReadOnly(false)
        .rememberAs(yourTurn = _),
      yourTurn.checkList(
        yourTurn.check("Create a Promoter called pTetR", "pTetR", "Promoter"),
        yourTurn.check("Create a DnaSequence called pTetRSeq with the DNA sequence " +
          "tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac (you may want to cut and paste)", "pTetRSeq", "DnaSequence"),
        yourTurn.check("Set the sequence property of pTetR equal to pTetRSeq", "pTetR", "sequence" -> "pTetRSeq")
      )
    ),
    Section(
      Heading.Level2("Cheaky sequences"),
      Paragraph(
        """Usually you will want to name your sequences separately from the components that use them.
          |However, shortbol is designed with quick prototyping of designs in mind, so it lets you be scruffy.
          |It actually allows you to crate the DNA sequence anonymously as the value of the sequence property.
          |Under the hood, this creates a new, anonymous DnaSequence instance and sets that as the value of the property.
          |It isn't best practice, but when deadlines loom, or when you are going to throw away the script anyway,
          |well, who's watching?
        """.stripMargin
      ),
      AceEditor(
        """pTetR : Promoter
          |    sequence : DnaSequence("tccctatcagtgatagagattgacatccctatcagtgatagagatactgagcac")
        """.stripMargin
      )
        .width(Length.Percentage(40))
        .height(Length.Pixel(60))
        .isReadOnly(true),
      Paragraph(
        """The main difference here is that instead of assigning a value to the sequence property, we are calling a type constructor.
          |So instead of using '=' to assign a value, we use ':' to construct one.
          |This pattern is used in a number of places in Shortbol scripts to build up nested instances within instances.
        """.stripMargin
      )
    )
  )

  var yourTurn: AceEditor = _
}
