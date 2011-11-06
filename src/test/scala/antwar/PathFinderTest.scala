package antwar.test

import antwar._
import foundation._

import org.scalatest._

abstract class PathFinderTest extends FunSuite {

  val extendWorld: Boolean

  protected def findPath(str: String): String = {
    val s = makeScene(str)
    val finder = new PathFinder(s.game.world, s.game.board.water.keySet)
    val path = finder.search(s.ant, s.food)
    val string = path map (_.compactString) getOrElse ""
    string.sorted
  }

  protected def makeScene(str: String): Scene = {

    val lines = str.lines.toList

    def find[A <: Positionable](symbol: Char): List[Tile] = {
      for {
        (line, row) <- (lines map (_.trim)).zipWithIndex
        (char, col) <- line.zipWithIndex
        if (char == symbol)
      } yield Tile(row, col)
    }.toList

    val rows = if (extendWorld) lines.size + 20 else lines.size
    val cols = if (extendWorld) lines.head.size + 20 else lines.head.size

    val board = Board(water = find('w'), myAnts = find('a'), enemyAnts = find('e'), food = find('f'), hills = find('h'))
    val game = makeGame(board, rows, cols)

    Scene(game, game.board.myAnts.values.head, game.board.food.values.head)
  }

  protected def makeGame(board: Board, rows: Int, cols: Int) = Game(
    turn = 42,
    board = board,
    parameters = GameParameters.dummy(rows = rows, cols = cols),
    memory = Memory.dummy,
    vision = Set()
  )

  protected case class Scene(game: Game, ant: MyAnt, food: Food) {
  }
}
