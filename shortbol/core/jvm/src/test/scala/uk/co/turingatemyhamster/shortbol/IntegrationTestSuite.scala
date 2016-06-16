package uk.co.turingatemyhamster.shortbol

import fastparse.all._
import fastparse.core.Mutable
//import fastparse.core.Result.Success
import fastparse.parsers.Terminals.{Start, End}
import utest._
import utest.framework.Test
import scalaz._
import Scalaz._
import uk.co.turingatemyhamster.shortbol.ops.DSL._

import ExpansionTestSuite.{checkExpansion, parse}

//object IntegrationTestSuite extends TestSuite {
//  override def tests = TestSuite {
//    * - checkExpansion(
//      parse("""import <https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol/genomic.sbol>
//              |p : Promoter""".stripMargin),
//      ProcessedImport(Url("https://raw.githubusercontent.com/drdozer/shortbolCommunity/master/sbol/genomic.sbol"), SBFile(Seq())) +:
//        parse("""p : ComponentDefinition
//                |  type = <SBOL:DNA>
//                |  role = <SBOL:Promoter>""".stripMargin)
//    )
//  }
//}
