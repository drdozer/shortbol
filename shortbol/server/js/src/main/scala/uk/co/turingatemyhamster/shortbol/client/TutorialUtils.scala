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
}
