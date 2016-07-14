package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML._

import pl.metastack.metarx.ReadChannel
import uk.co.turingatemyhamster.shortbol.ast._

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

  def check(editor: AceEditor, instId: Identifier, ass: Assignment): ReadChannel[Boolean] = {
    editor.lastSuccess.map { s =>
      (s.tops collect {
        case TopLevel.InstanceExp(i@InstanceExp(instId, _)) =>
          i.cstrApp.body
      }).flatten exists {
        case BodyStmt.Assignment(Assignment(p, v)) =>
          p == ass.property && v == ass.value
        case _ =>
          false
      }
    }
  }

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

  def DnaSequence = code("DnaSequence")

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
