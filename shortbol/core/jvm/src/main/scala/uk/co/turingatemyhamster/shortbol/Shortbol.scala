package uk.co.turingatemyhamster
package shortbol

import java.io.{FileWriter, File}
import javax.xml.stream.XMLOutputFactory

//import fastparse.core.Result.{Failure, Success}
import scopt.OptionParser
import uk.co.turingatemyhamster.datatree.io.RdfIo

import scala.io.Source

/**
 * Created by nmrp3 on 16/06/15.
 */
object Shortbol {

  val parser = new OptionParser[Config]("shortbol") {
    head("shortbol", "0.1")
    cmd("preprocess") text "preprocess shortbol into longbol" children {
      arg[File]("...") unbounded() required() text "shortbol files to preprocess" action { (f, c) =>
        val p0 = c.cmd match {
          case NoCmd => Preprocess()
          case p : Preprocess => p
          case _ =>
            reportError("Internal error")
            Preprocess()
        }
        val p1 = p0.copy(files = p0.files :+ f)
        c.copy(cmd = p1)
      }
    }
    cmd("export") text "export longbol to RDF" children {
      arg[File]("...") unbounded() required() text "longbol files to preprocess" action { (f, c) =>
        val ex0 = c.cmd match {
          case NoCmd => Export()
          case ex: Export => ex
          case _ =>
            reportError("Internal error")
            Export()
        }
        val ex1 = ex0.copy(files = ex0.files :+ f)
        c.copy(cmd = ex1)
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
      case Some(Config(ex : Export)) =>
        export(ex)
      case None =>
        // parser will have shown a usage error
    }

  }

  def preprocess(p: Preprocess): Unit = {
    ???
//    import Expander.ExpanderOps
//    for (file <- p.files) {
//      println(s"Processing $file")
//      Fixture.parser.SBFile.parse(Source.fromFile(file).mkString) match {
//        case f: Failure =>
//          System.err.println(f.traced)
//        case Success(sbf, _) =>
//          val out = new FileWriter(shortToLongFile(file))
//          val pp = Fixture.prettyPrinter(out)
//          for(expanded <- sbf.expansion.eval(Fixture.emptyContext))
//            pp.append(expanded)
//          out.close()
//      }
//    }
  }

  def export(ex: Export): Unit = {
    ???
//    for (file <- ex.files) {
//      println(s"Exporting $file to ${longToRdfFile(file)}")
//      Fixture.parser.SBFile.parse(Source.fromFile(file).mkString) match {
//        case f : Failure =>
//          System.err.println(f.traced)
//        case Success(lbf, _) =>
//          import datatree.ast._
//          val docRoot = Fixture.toDatatree[AstDatatree](lbf)
//          val writer = XMLOutputFactory.newInstance.createXMLStreamWriter(
//            new FileWriter(longToRdfFile(file)))
//          RdfIo.write(writer, docRoot)
//          writer.close()
//      }
//    }
  }

  def rewriteExtension(file: File, oldExt: String, newExt: String) =
    if(file.getName.endsWith(oldExt))
      new File(file.getCanonicalPath.replaceFirst(s"""\\.$oldExt$$""", s".$newExt"))
    else
      new File(s"${file.getCanonicalPath}.$newExt")

  def shortToLongFile(shortFile: File): File = rewriteExtension(shortFile, "sbol", "lbol")
  def longToRdfFile(longFile: File): File = rewriteExtension(longFile, "lbol", "rdf")
}

trait Cmd
case class Config(cmd: Cmd = NoCmd)

object NoCmd extends Cmd
case class Preprocess(files: Seq[File] = Seq()) extends Cmd
case class Export(files: Seq[File] = Seq()) extends Cmd