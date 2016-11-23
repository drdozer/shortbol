package uk.co.turingatemyhamster.shortbol
package terms

import shorthandAst.{sugar, NSPrefix}
import sugar._

/**
  * Created by nmrp3 on 23/11/16.
  */
object EDAM {
  val edam : NSPrefix = "edam"

  val fasta = edam :# "fasta"
  val genbank = edam :# "genbank"
}
