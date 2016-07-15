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
        aceExample(
          """@import <stdlib:sbol>
            |
            |pTetR : Promoter
            |
            |LacI : CDS""".stripMargin),
        Paragraph(v"""
              This script contains three lines.
              The first line imports the $sbol standard library.
              This tells $shortbol about $sbol, and pre-loads it with all the data types defined in the $sbol ${
          Anchor("standard")
            .url("http://sbolstandard.org/downloads/specifications/specification-data-model-2-0-1/")
            .title("SBOL Specification 2.0.1").attribute("target", "_blank")
        }. You will usually have this at the top of all your $shortbol scripts.
            The second line declares a promoter called $pTetR
        }, and the third declares a complement
              determining region (open reading frame, or coding region) called $LacI."""),
        Paragraph(v"""
              In this example we have separated each line with a blank line.
              Blank lines are ignored by $shortbol, but can make reading a script easier.
              This script, without the newlines means exactly the same thing.
          """),
        aceExample(
          """@import <stdlib:sbol>
            |pTetR : Promoter
            |LacI : CDS""".stripMargin),
        Paragraph(
          v"""I find that more difficult to read, but it's down to personal taste."""
        ),
        Paragraph(v"""
              Comments an be added to the script.
              Any line starting with a pound '${code("#")}' character is treated as a comment, and ignored.
              """),
        aceExample(
          """# Import the SBOL standard library
            |@import <stdlib:sbol>
            |
            |# Declare a promoter named pTetR
            |pTetR : Promoter
            |
            |# Declare a CDS named LacI
            |LacI : CDS""".stripMargin)
      ),
      Section(
        Heading.Level2("Adding some properties"),
        Paragraph(v"""
              So far we have created a promoter and a CDS and named them.
              In $shortbol, we call $pTetR and $LacI ${defn("instances")}.
              An instance is any thing that you have named as part of your description of your design.
              It may be a piece of DNA, or a large biological module, or a reference to a simulation, or perhaps a
               publication or co-worker.
              Instance names are case-sensative, so $pTetR and ${gene("PTetR")} are different instances, as the case
               of their leading letter differs.
              You can choose any name you like for an instance.
              The name is there to refer to it within your script.
              However, by choosing meaningful names, you will make the script easier to read and understand.
          """),
        Paragraph(v"""
              With $shortbol we can attach properties and values to instances.
              $shortbol uses indentation to make lines 'part of' a containing instance.
              For example, we can add a human-readable description to $pTetR like this:"""),
        aceExample(
          """pTetR : Promoter
            |  description = "pTet promoter"
          """.stripMargin),
        Paragraph(v"""
              We can also add comments inside an instance.
              These are indented, in the same way as the properties are.
          """),
        aceExample(
          """pTetR : Promoter
            |  # give pTetR a description
            |  description = "pTet promoter"
          """.stripMargin),
        Paragraph(v"""
             Comments ${emph("are not")} carried through to the final $sbol representation, so use them to document your script,
              for people who will read it in the future (probably you!) and may need some hints.
             Information needed to understand your deign, rather than your script, needs to be in properties
              like $description, as these ${emph("are")} available from $sbol.
          """),
        Paragraph(v"""
              You can add any names and values you like to an instance.
              It is perfectly fine for you to make new ones up as you need them.
              However, when we imported $sbol, that imported a library of many names that mean something special
               within the $sbol standard.
              You will frequently us these three $sbol properties for documenting your design:"""),
        List.Unordered(
          List.Item(v"""
              $description: associates a human-readable descriptions with things.
              This can be an extended block of text, that tells us more about an instance."""),
          List.Item(v"""
              $name: a human-readable name, possibly including spaces and special characters.
              For our $pTetR_gene, a good choice of name would be "pTetR"."""),
          List.Item(v"""
              $displayId: a formal identifier, usually originating from a database or catalogue,
               composed of only letters, numbers and underscores.
              In our $TetR_gene inverter example, the $pTetR part actually comes from the biobrick ${code("BBa_R0040")},
               so this is a good candidate for a $displayId value.""")
        ),
        Paragraph(v"""
              Here's our $pTetR_gene example documented with these properties filled in."""),
        aceExample(
          """pTetR : Promoter
            |  name = "pTetR"
            |  description = "pTetR promoter"
            |  displayId = "BBa_R0040"
          """.stripMargin),
        Paragraph(
          v"""
             You have probably spotted that we have used ${code("pTetR")} both for the instance name, and for the $name
              property.
             We did not have to do this.
             The two names are independent of each-other.
             The $name property captures a name that will be displayed to people and on diagrams.
             The instance name is just there so that we can refer to this promoter again within the script, so like the
             comments, won't be visible once the $shortbol script is processed into $sbol.
           """)
      ),
      Section(
        Heading.Level2("Your turn"),
        Paragraph(v"""
              Now it is your turn.
              So far all the code examples have been read-only.
              This time, however, you can edit the next code area.
              Click within it to give it focus and start typing.
              See if you can turn all the red ${code("todo")} items into green ${code("done")}s.
          """),
        Paragraph(v"""
              Start with the provided skeleton script and modify it so that $LacI has the $name "LacI", $description
                "LacI protein coding region" and $displayId of "P03023".
              Don't forget to indent the property names.
          """),
        aceTask(
          10,
          """LacI : CDS
            |""".stripMargin,
          check(
            v"""set $name to "LacI"""", "LacI", "name" -> slLit("LacI")),
          check(
            v"""set $description to "LacI protein coding region"""", "LacI", "description" -> slLit("LacI protein coding region")),
          check(
            v"""set $displayId to "P03023"""", "LacI", "displayId" -> slLit("P03023"))
        )
      )
    )
  }
}




