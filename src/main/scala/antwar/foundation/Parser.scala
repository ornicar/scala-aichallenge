package antwar.foundation

import annotation.tailrec
import io.Source

class Parser(source: Source) {

  def parseSetup = parseUntil("ready")
  def parseTurn = parseUntil("go")

  //@tailrec
  private def parseUntil(end: String): List[String] = {

    val lines = source.getLines

    def doParse: List[String] = lines.next.trim match {
      case "" => doParse
      case line if line == end => Nil
      case line => line :: doParse
    }

    doParse
  }
}
