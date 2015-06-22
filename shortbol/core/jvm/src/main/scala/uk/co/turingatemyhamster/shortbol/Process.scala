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
      (i => p.File.parse(i)) map
      {
        case Success(tls, _) =>
          tls
        case f : Failure.Mutable =>
          System.err.println(f.verboseTrace)
          Seq()
      }

    val all = inputs.flatten

    val cstrs = Ops.constructors(all)
    val inds = all collect { case i : InstanceExp => i }
    val ex = ExpansionContext(cstrs, Bindings(Map()))

    import Expander.ops._

    for {
      i <- inds
      ie <- i expandWith ex
    } pp.append(i)
    println
  }

}
