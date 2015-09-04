package uk.co.turingatemyhamster.shortbol

import java.io.{FileWriter, File}

import fastparse.core.Result.{Failure, Success}
import scopt.OptionParser

import scala.io.Source

/**
 * Created by nmrp3 on 16/06/15.
 */
object Shortbol {

  val parser = new OptionParser[Config]("shortbol") {
    head("shortbol", "0.1")
    cmd("preprocess") text("preprocess shortbol into longbol") children {
      arg[File]("...") unbounded() required() text("shortbol files to preprocess") action { (f, c) =>
        val p = c.cmd match {
          case NoCmd => Preprocess()
          case p : Preprocess => p
          case _ =>
            reportError("Internal error")
            Preprocess()
        }
        val p1 = p.copy(files = p.files :+ f)
        c.copy(cmd = p1)
      }
    }
    checkConfig {
      case Config(NoCmd) => failure("No command supplied")
      case _ => success
    }
  }

  def main(args: Array[String]): Unit = {

    parser.parse(args, Config()) match {
      case Some(Config(p : Preprocess)) =>
        preprocess(p)
      case None =>
        //parser.showUsageAsError
    }

  }

  def preprocess(p: Preprocess): Unit = {
    import Expander.ops._

    for (file <- p.files) {
      println(s"Processing $file")
      ShortbolParser.SBFile.parse(Source.fromFile(file).mkString) match {
        case f: Failure =>
          System.err.println(f.traced)
        case Success(sbf, _) =>
          val out = new FileWriter(shortToLongFile(file))
          val pp = new PrettyPrinter(out)
          for(expanded <- sbf.expansion.eval(ExpansionContext.empty))
            pp.append(expanded)
          out.close()
      }
    }
  }

  def shortToLongFile(shortFile: File): File =
    if(shortFile.getName.endsWith(".sbol"))
      new File(shortFile.getCanonicalPath.replaceFirst("""\.sbol$""", ".lbol"))
    else
      new File(shortFile.getCanonicalPath + ".lbol")
}

trait Cmd
case class Config(cmd: Cmd = NoCmd)

object NoCmd extends Cmd
case class Preprocess(files: Seq[File] = Seq()) extends Cmd