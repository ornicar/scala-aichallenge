package antwar

import foundation._
import org.scalatest._

class AstarTest extends FunSuite {

  type Pos = (Int, Int)
  type World = Map[Pos, Boolean]

  test("Find straight horizontal path") {

    assertPath(""".......   .......
                  .a....b   .a+++++
                  .......   .......""")
  }

  test("Find straight vertical path") {

    assertPath("""..a....   ..a....
                  .......   ..+....
                  .......   ..+....
                  .......   ..+....
                  .......   ..+....
                  ..b....   ..+....""")
  }

  test("Find diagonal path") {

    assertPath(""".a.....   .a.....
                  .......   .++....
                  .......   ..+....
                  .......   ..+....
                  .....b.   ..++++.""")
  }

  test("Find horizontal with tight maze path") {

    assertPath("""....#.b   ++++#.+
                  .##.#..   +##+#.+
                  ..#.##.   +.#+##+
                  a.#....   a.#++++
                  ..#.#..   ..#.#..""")
  }

  test("Find horizontal loose maze path") {

    assertPath(""".......   .......
                  ....#..   ....#..
                  a...#..   a...#..
                  ...##.b   +++##.+
                  .......   ..+++++""")
  }

  private def assertPath(string: String) {
    val (worldString, solutionString) = splitString(string)
    val world = makeWorld(worldString)
    val from = find(worldString, 'a')
    val to = find(worldString, 'b')
    val solution = Astar.search(world, from, to)
    val sw = drawSolution(worldString, solution, '+')
    assert("\n" + sw === "\n" + solutionString)
  }

  private def splitString(string: String) = (
    string.lines map (_.trim) map (_.takeWhile(' '!=)) mkString "\n",
    string.lines map (_.trim) map (_.reverse) map (_.takeWhile(' '!=)) map (_.reverse) mkString "\n"
  )

  private def drawSolution(w: String, pos: List[Pos], char: Char) = {
    val chars = (w.lines map (_.toArray)).toArray
    pos foreach { case(row, col) => chars(row)(col) = char }
    (chars map (_.mkString)) mkString "\n"
  }

  private def find(w: String, symbol: Char): Pos = {
    for {
      (line, row) <- w.lines.toList.zipWithIndex
      (char, col) <- line.toList.zipWithIndex
      if (char == symbol)
    } yield (row, col)
  }.head

  private def makeWorld(w: String): World = {
    for {
      (line, row) <- w.lines.toList.zipWithIndex
      (char, col) <- line.toList.zipWithIndex
      isWall = char == '#'
    } yield ((row, col), isWall)
  }.toMap

}
