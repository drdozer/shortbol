package uk.co.turingatemyhamster.shortbol.client

import org.widok._
import org.widok.bindings.HTML

import scala.concurrent.Future

/**
  *
  *
  * @author Matthew Pocock
  */
case class TutorialNotFound() extends Page {
  override def render(route: InstantiatedRoute) = Future.successful {
    import HTML._
    Container.Generic(
      Heading.Level1("Page not found"),
      Paragraph(s"Could not find page ${route.uri()}")
    )
  }
}
