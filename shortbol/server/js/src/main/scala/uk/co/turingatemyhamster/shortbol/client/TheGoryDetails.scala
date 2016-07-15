package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML._
import TutorialUtils._

/**
  *
  *
  * @author Matthew Pocock
  */
class TheGoryDetails {
  Section(
    Heading.Level2(v"A diversion: $shortbol sugar"),
    Paragraph(
      v"""
         In previous tutorials, we used a range of types of genetic parts, like $Promoter and $Terminator.
         Within $sbol, each of these is actually represented as a $ComponentDefinition instance.
         $shortbol takes care of re-writing $Promoter, and all the other genetic parts that you've been using to
          the $ComponentDefinition that $sbol expects to see.
         You could, if you had wanted, constructed an instance of $ComponentDefinition and filled in its properties
          to tell SBOL what kind of component it was, but this process is fiddly and error-prone, so $shortbol
          provides various type constructors to hide this complexity for you.
         This 'syntactic sugar'
       """
    ),
    aceExample(
      """@import stdlib:sbol
        |
        |# A promoter, maximum shortbol sugar
        |p1 : Promoter
        |
        |# A promoter, shortbol with some sugar
        |p2 : DnaComponent
        |  role = Promoter
        |
        |# A promoter, sbol with some sugar
        |p2 : ComponentDefinition
        |  type = DNA
        |  role = Promoter
        |
        |# A promoter, sbol with no sugar
        |p3 : ComponentDefinition
        |  type = biopax:DnaRegion
        |  role = <http://identifiers.org/so/SO:0000167>
      """.stripMargin),
    Paragraph(
      v"""
         The instances ${code("p1")}, ${code("p2")}, ${code("p3")} and ${code("p4")} are equivalent.
         You could choose to write down your promoter in any of these four forms, but I hope you agree that the more
          sugary forms are easier to understand and to write.
       """
    )
  )

}
