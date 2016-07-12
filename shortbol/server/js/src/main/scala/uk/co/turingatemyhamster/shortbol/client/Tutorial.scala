package uk.co.turingatemyhamster.shortbol.client

import org.widok._

import scala.concurrent.Future

/**
  *
  *
  * @author Matthew Pocock
  */
object TutorialRoutes {
  val routes =
    Route("/", () => TutorialPage(TutorialRoot())) ::
      Route("/yourFirstScript", () => TutorialPage(YourFirstScript())) ::
      Route("/introductionToTypes", () => TutorialPage(IntroductionToTypes())) ::
      Route("/addingSequences", () => TutorialPage(AddingSequences())) ::
      Nil

  val notFound = Route("/404", TutorialNotFound)
}

object Tutorial extends RoutingApplication(
  TutorialRoutes.routes.to[Set],
  TutorialRoutes.notFound
)

trait TutorialContent {
  def navigationEntry: View
  def render(route: InstantiatedRoute): View
}

case class TutorialPage(content: TutorialContent) extends Page {

  override final def render(route: InstantiatedRoute) = Future.successful {
      import org.widok.bindings.HTML._
      Container.Generic(
        Header(
          Image("images/logo.png").css("logo"),
          Heading.Level1("Shortbol Tutorial")
        ),
        Container.Generic(
          Container.Generic(
            Navigation(
              List.Unordered(
                TutorialRoutes.routes.map { case r@Route(path, page) =>
                  page() match {
                    case TutorialPage(tc) =>
                      List.Item(Anchor(tc.navigationEntry).url(r()))
                  }
                } :_*
              )
            ),
            content.render(route)
          )
        ).css("tutorialContent"),
        Footer(
          List.Unordered(
            List.Item(Container.Inline(Anchor(
              Image("images/newcastle-university.png")
            ).url("http://ncl.ac.uk/"))),
            List.Item(Container.Inline(Anchor(
              Image("images/icos.png")
            ).url("http://www.ncl.ac.uk/computing/research/groups/icos/"))),
            List.Item(Container.Inline(Anchor(
              Container.Inline("View on Github"),
              Image("images/GitHub-Mark-32px.png")).url("https://github.com/drdozer/shortbol")))
          )
        )
      )
    }

}