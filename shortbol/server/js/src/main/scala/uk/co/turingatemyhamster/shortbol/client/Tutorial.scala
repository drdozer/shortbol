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
    Route("/", () => TutorialPage(Introduction())) ::
      Route("/yourFirstScript", () => TutorialPage(YourFirstScript())) ::
      Route("/introductionToTypes", () => TutorialPage(IntroductionToTypes())) ::
      Route("/addingSequences", () => TutorialPage(AddingSequences())) ::
      Route("/composingDesigns", () => TutorialPage(ComposingDesigns())) ::
      Nil

  val nav = {
    val rO = routes map Some.apply
    (None :: rO) zip (rO ::: None :: Nil)
  }

  def before(at: Route): Option[Route] =
    nav collectFirst { case (l, r) if r contains at => l } flatten

  def after(at: Route): Option[Route] =
    nav collectFirst { case (l, r) if l contains at => r } flatten

  def find(p: String): Route = routes filter { _.path endsWith p } head

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
  import org.widok.bindings.HTML._
  import TutorialUtils._

  def prevNext(route: InstantiatedRoute) = {
    val before = TutorialRoutes.before(route.route)
    val after = TutorialRoutes.after(route.route)

    val backTxt = "back" : View
    val forwardTxt = "forward" : View

    val back = before map (r => Anchor(backTxt).url(r())) getOrElse backTxt
    val forward = after map (r => Anchor(forwardTxt).url(r())) getOrElse forwardTxt

    Container.Generic(back, " ", forward).css("back_forward")
  }

  override final def render(route: InstantiatedRoute) = Future.successful {
    Container.Generic(
      Header(
        Image("images/logo.png").css("logo"),
        Heading.Level1(v"$shortbol Tutorial")
      ),
      Container.Generic(
        Navigation(
          Heading.Level2("Tutorials"),
          List.Unordered(
            TutorialRoutes.routes.map { case r@Route(path, page) =>
              page() match {
                case TutorialPage(tc) =>
                  List.Item(Anchor(tc.navigationEntry).url(r()))
              }
            } :_*
          )
        ).css("navpannel"),
        content.render(route),
        prevNext(route)
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