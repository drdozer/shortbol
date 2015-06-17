package uk.co.turingatemyhamster.shortbol

import fastparse.core.Result.{Failure, Success}

import scala.io.Source

/**
 * Created by nmrp3 on 16/06/15.
 */
object Process {

  def main(args: Array[String]): Unit = {

    val p = new ShortbolParser()
    val pp = new PrettyPrint(System.out)

    val inputs = args map
      Source.fromFile map
      (_.mkString) map
      (i => p.TopLevels.parse(i)) foreach
      {
        case Success(tls, _) =>
          pp.append(tls)
        case f : Failure.Mutable =>
          System.err.println(f.verboseTrace)
      }

  }

}
