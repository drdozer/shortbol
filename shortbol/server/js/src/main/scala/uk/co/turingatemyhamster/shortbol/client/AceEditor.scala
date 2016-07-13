package uk.co.turingatemyhamster.shortbol.client

import org.scalajs.dom.Node
import org.widok._
import pl.metastack.metarx._
import uk.co.turingatemyhamster.shortbol.ast._
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser
import ShortbolParser.POps
import fastparse.core.Parsed.Success
import org.widok.bindings.HTML._

import scala.scalajs.js
import scala.util.Random

import TutorialUtils._

case class AceEditor(src: String*) extends Widget[AceEditor] {
  lazy val readOnly = Var[Boolean](false)

  def isReadOnly(ro: Boolean) = {
    readOnly := ro
    this
  }

  lazy val editorText: Var[String] = {
    val text = Var[String]("")
    editor.getSession().on("change", (e: AnyVal) => text := editor.getSession().getValue().asInstanceOf[String])
    text
  }

  lazy val parsed = editorText map { txt =>
    ShortbolParser.SBFile.withPositions(LocalName("shortbol_editor"), txt) }

  lazy val lastSuccess = parsed collect {
    case s: Success[SBFile] =>
      s.value }

  override val rendered = DOM.createElement("div", Seq(src.mkString("\n")))

  var editor: js.Dynamic = _

  override def render(parent: Node,
                      offset: Node) = {
    super.render(parent, offset)
    js.Dynamic.global.window.setTimeout(registerAce _, 0)
  }

  def registerAce() = {
    lazy val ace = js.Dynamic.global.ace
    if(rendered.id == null || rendered.id.isEmpty) rendered.id = Random.alphanumeric.take(10).mkString
    editor = ace.edit(rendered.id)
    editor.setTheme("ace/theme/solarized_light")
    editor.getSession().setMode("ace/mode/sbolshorthand2")
    editor.setReadOnly(readOnly.get)

    readOnly.attach(ro => editor.setReadOnly(ro))
  }

  def checkList(check: SbolCheck*) = List.Unordered(check map { c =>
    List.Item(
      Container.Inline(c.description).css("description"), ": ",
      Container.Inline("todo").css("warning").delayed(_.show(!c.checker)),
      Container.Inline("done").css("success").delayed(_.show(c.checker))
    )
   } :_*).css("checkList")

  case class AssignmentSbolCheck(description: String, instId: Identifier, ass: Assignment) extends SbolCheck {
    lazy val checker = lastSuccess.map { s =>
      (s.tops collect {
        case TopLevel.InstanceExp(i@InstanceExp(id, _)) if id == instId =>
          println(s"Found instance with id $instId")
          i.cstrApp.body
      }).flatten exists {
        case BodyStmt.Assignment(Assignment(p, v)) =>
          println(s"Comparing ($p -> $v) with (${ass.property} -> ${ass.value}) : (${p == ass.property} -> ${v == ass.value})")
          p == ass.property && v == ass.value
        case _ =>
          false
      }
    }
  }

  case class TypeSbolCheck(description: String, instId: Identifier, ofType: Identifier) extends SbolCheck {
    lazy val checker = lastSuccess.map {
      s =>
        (s.tops collect {
          case TopLevel.InstanceExp(i@InstanceExp(id, ConstructorApp(TpeConstructor1(tpeName, _), _))) =>
            id == instId && tpeName == ofType
        }).exists(_ == true)
    }
  }

  def check(description: String, instId: Identifier, ass: Assignment) = AssignmentSbolCheck(description, instId, ass)

  def check(description: String, instId: Identifier, ofType: Identifier) = TypeSbolCheck(description, instId, ofType)
}

sealed trait SbolCheck {
  def description: String
  def checker: ReadChannel[Boolean]
}
