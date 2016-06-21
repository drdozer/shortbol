package uk.co.turingatemyhamster.shortbol.ops

import fastparse.core.{Parser, Parsed}
import fastparse.parsers.Combinators.Rule


/**
  * Created by nmrp3 on 21/06/16.
  */
case class ParseStack(from: Int, to: Int, ruleName: String, children: List[ParseStack])

class StackBuilder {
  var tops: List[ParseStack] = Nil

  def instrument(rule: Parser[_], from: Int, result: () => Parsed[_]): Unit = result() match {
    case s : Parsed.Success[_] =>
      val to = s.index

      val poppedInvalidated = tops dropWhile (p => p.to > to)
      val (contained, uncontained) = poppedInvalidated span (p => p.from >= from)
      val ps = ParseStack(from, to, rule.asInstanceOf[Rule[_]].name, contained.reverse)
      tops = ps :: uncontained
    case _ =>
  }
}