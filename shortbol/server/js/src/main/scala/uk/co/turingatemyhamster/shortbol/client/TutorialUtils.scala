package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML._

import pl.metastack.metarx.ReadChannel
import uk.co.turingatemyhamster.shortbol.shorthandAst._

import scala.scalajs.js

/**
  *
  *
  * @author Matthew Pocock
  */
object TutorialUtils {

  implicit class NodeMemoriser[N <: Node](val _n: N) extends AnyVal {
    def rememberAs(f: N => Unit): N = {
      f(_n)
      _n
    }

    def delayed[T](f: N => T): N = {
      js.Dynamic.global.window.setTimeout(() => f(_n), 100)
      _n
    }
  }

  val aceLineHeight = 12.5

  def aceExample(code: String) =
    AceEditor(code).css("code_example").height(Length.Pixel(code.lines.length * aceLineHeight)).isReadOnly(true)

  def aceTask(lineCount: Int, code: String, check: (AceEditor => SbolCheck)*) = {
    var editor: AceEditor = null
    var taskList: TaskList = null

    Container.Generic(
      AceEditor(code)
        .height(Length.Pixel(lineCount * aceLineHeight))
        .isReadOnly(false)
        .css("editor")
        .rememberAs(editor = _),
      Container.Generic(
        TaskList(check map (_ apply editor) :_*).rememberAs(taskList = _)
      ).css("tasks"),
      Container.Generic(
        Container.Generic(
          Container.Inline("Good").css("success").show(editor.parseStatus),
          Container.Inline("Errors").css("warning").show(!editor.parseStatus)
        ).css("parse_status"),
        Container.Inline("Completed!").css("success").visible(taskList.allCompleted && editor.parseStatus).css("completion"),
        Container.Generic(
          taskList.progress map { case (done, of) => s"Progress: $done/$of" }).css("progress")
      ).css("status_bar"),
      Container.Generic().css("force_clear")
    ).css("exercise")
  }

  def check(description: View, instId: Identifier, ass: Assignment) =
    (ae: AceEditor) => ae.AssignmentSbolCheck(description, instId, ass)

  def check(description: View, instId: Identifier, ofType: Identifier) =
    (ae: AceEditor) => ae.TypeSbolCheck(description, instId, ofType)

  def check(description: View, instId: Identifier, args: List[ValueExp]) =
    (ae: AceEditor) => ae.ConstructorSbolCheck(description, instId, args)

  def check(description: View, instId: Identifier, prop: Identifier, tpe: Identifier, args: List[ValueExp]) =
    (ae: AceEditor) => ae.NestedConstructorSbolCheck(description, instId, prop, tpe, args)


  def projectName(v: View*) = Container.Inline(v :_*).css("projectName")
  def code(v: View*) = Container.Inline(v :_*).css("code")
  def gene(v: View*) = Container.Inline(v :_*).css("gene")
  def defn(v: View*) = Container.Inline(v :_*).css("definition")
  def emph(v: View*) = Container.Inline(v :_*).css("emph")

  def shortbol = projectName("ShortBOL")
  def sbol = projectName("SBOL")
  def edam = projectName("EDAM")

  def TetR_gene = gene("TetR")
  def pTetR_gene = gene("pTetR")
  def LacI_gene = gene("LacI")

  def pTetR = code("pTetR")
  def pTetRSeq = code("pTetRSeq")
  def LacI = code("LacI")
  def lacIRbs = code("lacIRbs")
  def lacIT = code("lacIT")
  def lacITSeq = code("lacITSeq")

  def displayId = code("displayId")
  def description = code("description")
  def name = code("name")
  def sequence = code("sequence")
  def sequenceConstraint = code("sequenceConstraint")
  def sequenceAnnotation = code("sequenceAnnotation")
  def `type` = code("type")
  def component = code("component")
  def at = code("at")
  def inline = code("inline")
  def reverseComplement = code("reverseComplement")

  def precedes = code("precedes")
  def sameOrientationAs = code("sameOrientationAs")
  def differentOrientationAs = code("differentOrientationAs")

  def DnaSequence = code("DnaSequence")
  def DnaComponent = code("DnaComponent")
  def ComponentDefinition = code("ComponentDefinition")
  def SequenceConstraint = code("SequenceConstraint")

  def Promoter = code("Promoter")
  def CDS = code("CDS")
  def Terminator = code("Terminator")
  def RBS = code("RBS")
  def Operator = code("Operator")


  implicit class WidokStringContext(val _sc: StringContext) extends AnyVal {
    def v(args: Any*): View = if(args.isEmpty) {
      _sc.parts.mkString: View
    } else {
      Container.Inline(
        (
          (_sc.parts.head : View) :: (args zip _sc.parts.tail.map(p => p : View)).to[List].flatMap {
            case (v, p) => (v match {
              case vv : View => vv
              case vs => vs.toString : View
            }) :: p :: Nil
          }
          ) :_*
      )
    }
  }
}
