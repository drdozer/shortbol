package uk.co.turingatemyhamster.shortbol

import fastparse.core.Result.{Failure, Success}

import scala.io.Source

/**
 * Created by nmrp3 on 16/06/15.
 */
object Process {

  def main(args: Array[String]): Unit = {

    val p = new ShortbolParser()
    val pp = new PrettyPrinter(System.out)

    val inputs = args map
      Source.fromFile map
      (_.mkString) map
      (i => p.File.parse(i)) map
      {
        case Success(tls, _) =>
          tls
        case f : Failure =>
          System.err.println(f.traced)
          Seq()
      }

    val all = inputs.flatten

    println("all")
    println(all.toList)

    val cstrs = Ops.constructors(all)
    val inds = all collect { case i : InstanceExp =>
      println("found individual " + i)
      i
    }
    val ex = ExpansionContext(cstrs, Bindings(Map()))

    println("individuals")
    println(inds.toList)

    import Expander.ops._

    println("expansions")
    for {
      i <- inds
      _ = println("Expanding individual: " + i)
      ie <- i expandWith ex
    } {
      println("Indiviudal:")
      pp.append(i)
      println("Expansion:")
      pp.append(ie)
      println()
    }
  }

}
