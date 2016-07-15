package uk.co.turingatemyhamster.shortbol.client
import org.widok._
import org.widok.bindings.HTML._
import TutorialUtils._
import uk.co.turingatemyhamster.shortbol.ast._
import uk.co.turingatemyhamster.shortbol.ast.sugar._

/**
  *
  *
  * @author Matthew Pocock
  */
case class ComposingDesigns() extends TutorialContent {
  override def navigationEntry = "Composing Designs"

  override def render(route: InstantiatedRoute) = Container.Generic(
    Section(
      Heading.Level2("Composition"),
      Paragraph(
        v"""
           A core principle of synthetic biology design is that larger designs are built up from smaller, well-validated
            components.
           This paradigm is exemplified by ${
          Anchor("BioBricks")
            .url("http://biobricks.org/")
            .title("The BioBricks Foundation")
            .attribute("target", "_blank")}, an assembly standard and parts registry of genomic parts.
            The $sbol data standard provides a lot of tooling for describing how a design is composed.
            However, while $sbol is both flexible and precise, it pays for that in verbosity and complexity.
         """),
      Paragraph(
        v"""
           In this tutorial, we are going to look at several strategies for using $shortbol to compose a larger design
            from smaller ones, by building up the $TetR_gene inverter from its component parts.
           We will also look at short-cuts that $shortbol allows you to take, compared to the full $sbol data model.
         """
      )
    ),
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
    ),
    Section(
      Heading.Level2("Sub-components"),
      Paragraph(v"""In the tutorial ${
        val itt = TutorialRoutes.find("introductionToTypes")
        val TutorialPage(tc) = itt.page()
        Anchor(tc.navigationEntry).url(itt())
      }, we made instances for the four parts of the $TetR_gene inverter device.
        However, we stopped short of assembling them into a composite device."""),
      Paragraph(
        v"""
           $sbol represents both composite devices and simple genetic parts as instances of $ComponentDefinition.
           In our case, as this is a DNA design, rather than an RNA or protein design, we can use the $shortbol
           $DnaComponent type.
           As we saw above, $DnaComponent is sugar for a $ComponentDefinition with the ${`type`} field set
            to ${code("DNA")}.
         """),
      Paragraph(
        v"""
          To place the genetic parts we've made within a larger $DnaComponent, we use the $component property.
          """
      ),
      aceExample(
              """@import stdlib:sbol
                |
                |# The genetic parts of the TetR inverter
                |pTetR   : Promoter
                |lacIRbs : RBS
                |LacI    : CDS
                |lacIT   : Terminator
                |
                |# The composite device for the TetR inverter
                |tetRInverter : DnaComponent
                |  component = pTetR
                |  component = lacIRbs
                |  component = LacI
                |  component = lacIT
              """.stripMargin),
      Paragraph(
        v"""
           Because we are adding four sub-components, we set the $component property four times.
           When you assign to a property multiple times, you add new values rather than over-writing previous ones.
         """
      )
    ),
    Section(
      Heading.Level2("Constraints"),
      Paragraph(
        v"""
           We have built a $pTetR_gene inverter device that contains its four genetic parts as sub-components.
           However, we haven't specified anything about how these parts are to be assembled.
           There are two complementary ways to specify this.
           Firstly, we can attach constraints on their relative positions.
           Secondly, we can say exactly where the sub-components are located within the composite component.
           In this section we are going to explore constraints.
         """),
      Paragraph(
        v"""
           Sequence constraints are declared using the $sequenceConstraint property.
           The values of this property are $SequenceConstraint instances.
           These can be a bit fiddly to set up, which is why $sbol provides you with lots of sugar to make the medicine
            go down.
         """
      ),
      Paragraph(
        v"""
           The constraint we need in this design is called $precedes.
           This says that one component comes before the other in the design.
           In this way, we can place the genetic parts, left-to-right.
         """
      ),
      aceExample(
              """# The composite device for the TetR inverter
                |tetRInverter : DnaComponent
                |  # include the child components
                |  component = pTetR
                |  component = lacIRbs
                |  component = LacI
                |  component = lacIT
                |  # relative positions of child components
                |  sequenceConstraint = pTetR   precedes lacIRbs
                |  sequenceConstraint = lacIRbs precedes LacI
                |  sequenceConstraint = LacI    precedes lacIT
              """.stripMargin),
      Paragraph(
        v"""
           You may be thinking that it is a bit of a faff listing the child genetic parts both as components and within
            the constraints.
           We agree.
           You can cut this declaration down even further by missing out the $component assignments.
           $shortbol will notice that you have described the relative placement of child components that aren't listed
           as child components, and add them back in for you.
           So an equivalent, but shorter $shortbol instance declaration would be:
         """
      ),
      aceExample(
        """# The composite device for the TetR inverter
          |tetRInverter : DnaComponent
          |  # relative positions of child components
          |  sequenceConstraint = pTetR   precedes lacIRbs
          |  sequenceConstraint = lacIRbs precedes LacI
          |  sequenceConstraint = LacI    precedes lacIT
          |        """.stripMargin),
      Paragraph(
        v"""
           $sbol currently defines three types of constraints.
           These are $precedes, which we have already met, $sameOrientationAs and $differentOrientationAs.
           These last two tell you if the two components share the same orientation or have different orientations, but
            not what the orientation of either component is.
         """
      ),
      Paragraph(
        v"""
           You may have spotted that the way we used $precedes was to place it between the two things it was relating.
           This is called ${emph("infix")} notation, which just means 'inbetween'.
           Infix notation is sugar for a constructor that expects exactly two values.
           It lets us write down things in $shortbol scripts that read a bit more like natural language, and a bit less
            like computer code.
         """
      ),
      aceExample(
        """dc : DnaComponent
          |
          |  # using infix notation
          |  sequenceConstraint = promoter precedes open_reading_frame
          |
          |  # the same thing, but using a constructor
          |  sequenceConstraint : precedes(promoter, open_reading_frame)
        """.stripMargin
      ),
      Paragraph(
        v"""
            Which of these two forms you use is entirely up to you.
            I find that infix notation often results in scripts that are clearer, and easier to read, but it's
             ultimately a personal choice what style you use.
          """
      )
    ),
    Section(
      Heading.Level2("Locations"),
      Paragraph(
        v"""
           In the previous section we saw how $shortbol can describe the relative positions of children within a parent
            design.
           Here we will see how it can give them exact positions.
         """
      ),
      Paragraph(
        v"""
           The $sbol property used to position sub-components is called $sequenceAnnotation.
         """
      ),
      aceExample(
        """# The composite device for the TetR inverter
          |tetRInverter : DnaComponent
          |  # absolute positions of child components
          |  sequenceAnnotation : at(pTetR,      1,   55, inline)
          |  sequenceAnnotation : at(lacIRbs,   56,   68, inline)
          |  sequenceAnnotation : at(LacI,      69, 1197, inline)
          |  sequenceAnnotation : at(lacIT,   1198, 1288, inline)
        """.stripMargin),
      Paragraph(
        v"""
           Let's unpack that a bit.
           The value of $sequenceAnnotation is actually a complex object, with quite a few properties that need to be
            set up just-so.
           To avoid you needing to get all these details right, $shortbol provides a constructor named $at to handle
            it for you.
           This expects four values; the $component to locate, the start and end coordinates, and a flag indicating if
            the construct is to be instered $inline (on the forward strand), or $reverseComplement (on the reverse
            backward strand).
           In the example above, I have made use of $shortbol's lack of interest in how many spaces are used to line up
            these four values into a table of sorts, to aid with reading.
         """)
    ),
    Section(
      Heading.Level2("Your turn"),
      Paragraph(
        v"""Now it's your turn.
           In the previous $pTetR_gene inverter positions example, we constrained the positions of the four parts by
           saying they preceded one-another.
           This time, we will constrain them all to be in the same orientation.
           Use $sameOrientationAs on $pTetR, $lacIRbs, $LacI and $lacIT to ensure that they are all on the same strand.
         """
      ),
      aceTask(9,
      """dc : DnaComponent
        |  # sequenceConstraint : sameOrientationAs(a, b)
        |  """.stripMargin,
        check(v"$pTetR and $lacIRbs have the same orientation", "dc", "sequenceConstraint", "sameOrientationAs", "pTetR"::("lacIRbs" : ValueExp)::Nil),
        check(v"$lacIRbs and $LacI have the same orientation", "dc", "sequenceConstraint", "sameOrientationAs", "lacIRbs"::("LacI" : ValueExp)::Nil),
        check(v"$LacI and $lacIT have the same orientation", "dc", "sequenceConstraint", "sameOrientationAs", "LacI"::("lacIT": ValueExp)::Nil)
      )
    )
  )
}
