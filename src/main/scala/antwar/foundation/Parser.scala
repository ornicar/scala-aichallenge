package antwar.foundation

import annotation.tailrec
import io.Source

class Parser(source: Source) {

  def setup = until("ready")
  def turn = until("go")

  //@tailrec
  private def until(end: String): List[String] = {

    val lines = source.getLines

    def parse: List[String] = lines.next.trim match {
      case "" => parse
      case line if line == end => Nil
      case line => line :: parse
    }

    parse
  }
}
