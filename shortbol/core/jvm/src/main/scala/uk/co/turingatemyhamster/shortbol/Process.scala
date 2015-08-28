package uk.co.turingatemyhamster.shortbol

import fastparse.core.Result.{Failure, Success}

import scala.io.Source

/**
 * Created by nmrp3 on 16/06/15.
 */
object Process {

  def main(args: Array[String]): Unit = {

    import Expander.ops._
    val pp = new PrettyPrinter(System.out)

    for (arg <- args) {
      ShortbolParser.SBFile.parse(Source.fromFile(arg).mkString) match {
        case f: Failure =>
          System.err.println(f.traced)
        case Success(sbf, _) =>
          val expanded = sbf.expansion.eval(ExpansionContext.empty)
      }
    }
  }
}