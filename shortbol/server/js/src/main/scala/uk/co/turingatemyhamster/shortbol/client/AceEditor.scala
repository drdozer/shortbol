package uk.co.turingatemyhamster.shortbol.client

import org.scalajs.dom.Node
import org.widok._
import pl.metastack.metarx._
import uk.co.turingatemyhamster.shortbol.sharedAst._
import uk.co.turingatemyhamster.shortbol.shorthandAst._
import uk.co.turingatemyhamster.shortbol.ops.ShortbolParser
import ShortbolParser.POps
import fastparse.core.Parsed.Success
import org.widok.bindings.HTML._

import scala.scalajs.js
import scala.util.Random
import TutorialUtils._
import org.scalajs.dom.html.Element

import scala.concurrent.Promise

case class AceEditor(src: String*) extends Widget[AceEditor] {
  import scala.concurrent.ExecutionContext.Implicits.global

  lazy val readOnly = Var[Boolean](false)

  def isReadOnly(ro: Boolean) = {
    readOnly := ro
    this
  }

  lazy val editorText: Var[String] = {
    val text = Var[String]("")
    for(ed <- editor) ed.getSession().on("change", (e: AnyVal) => text := ed.getSession().getValue().asInstanceOf[String])
    text
  }

  lazy val parsed = editorText map { txt =>
    ShortbolParser.SBFile.withPositions(LocalName("shortbol_editor"), txt) }

  lazy val parseStatus = parsed map {
    _ match {
      case Success(_, _) => true
      case _ => false
    }
  }

  lazy val lastSuccess = parsed collect {
    case s: Success[SBFile] =>
      s.value }

  override val rendered = DOM.createElement("div", Seq(src.mkString("\n")))

  val editorP = Promise[js.Dynamic]
  def editor = editorP.future

  override def render(parent: Node,
                      offset: Node) = {
    super.render(parent, offset)
    js.Dynamic.global.window.setTimeout(registerAce _, 0)
  }

  def registerAce() = {
    lazy val ace = js.Dynamic.global.ace
    if(rendered.id == null || rendered.id.isEmpty) rendered.id = Random.alphanumeric.take(10).mkString
    val ed = ace.edit(rendered.id)
    ed.setTheme("ace/theme/solarized_light")
    ed.getSession().setMode("ace/mode/sbolshorthand2")
    ed.setReadOnly(readOnly.get)

    readOnly.attach(ro => ed.setReadOnly(ro))

    editorP.complete(scala.util.Success(ed))
  }

  case class PropertyExpSbolCheck(description: View, instId: Identifier, pe: PropertyExp) extends SbolCheck {
    lazy val checker = lastSuccess.map { s =>
      (s.tops collect {
        case TopLevel.InstanceExp(i@InstanceExp(id, _)) if id == instId =>
          println(s"Got an instance with matching ID $id == $instId")
          i.cstrApp.body
      }).flatten exists {
        case BodyStmt.PropertyExp(PropertyExp(p, v)) =>
          println(s"Comparing: $p == ${pe.property} ~> ${p == pe.property}")
          println(s"Comparing: $v == ${pe.value} ~> ${v == pe.value}")
          p == pe.property && v == pe.value
        case _ =>
          false
      }
    }
  }

  case class TypeSbolCheck(description: View, instId: Identifier, ofType: Identifier) extends SbolCheck {
    lazy val checker = lastSuccess.map {
      s =>
        (s.tops collect {
          case TopLevel.InstanceExp(i@InstanceExp(id, ConstructorApp(TpeConstructor1(tpeName, _), _))) =>
            id == instId && tpeName == ofType
        }).contains(true)
    }
  }

  case class ConstructorSbolCheck(description: View, instId: Identifier, args: Seq[ValueExp]) extends SbolCheck {
    lazy val checker = lastSuccess.map {
      s =>
        (s.tops collect {
          case TopLevel.InstanceExp(i@InstanceExp(id, ConstructorApp(TpeConstructor1(_, a), _))) =>
            id == instId && a == args
        }).contains(true)
    }
  }

  case class NestedConstructorSbolCheck(description: View, instId: Identifier, prop: Identifier, tpe: Identifier, args: List[ValueExp]) extends SbolCheck {
    lazy val checker = lastSuccess.map {
      s =>
        (for {
          TopLevel.InstanceExp(InstanceExp(id, ConstructorApp(TpeConstructor1(_, _), c))) <- s.tops if id == instId
          BodyStmt.PropertyExp(PropertyExp(bi, PropertyValue.Nested(ConstructorApp(TpeConstructor1(t, a), _)))) <- c if {
          bi == prop && t == tpe && a == args}
        } yield ()).nonEmpty
    }
  }
}

sealed trait SbolCheck {
  def description: View
  def checker: ReadChannel[Boolean]
}

case class TaskList(check: SbolCheck*) extends Widget[TaskList] {
  lazy val allCompleted = check.map(_.checker).reduce(_ && _)

  lazy val progress = check.map(_.checker.map(b => if(b) 1 else 0)).reduce(_ + _).map(c => (c, check.length))

  override val rendered =
    List.Unordered(
      check map { c =>
        List.Item(
          Container.Inline(c.description).css("description"), ": ",
          Container.Inline("todo").css("warning").delayed(_.show(!c.checker)),
          Container.Inline("done").css("success").delayed(_.show(c.checker))
        )
      } :_*)
      .css("taskList").rendered
}