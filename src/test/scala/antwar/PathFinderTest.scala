package antwar

import foundation._

import org.scalatest._

abstract class PathFinderTest extends FunSuite {

  protected def findPath(str: String): String = {
    val s = makeScene(str)
    val finder = PathFinder(s.game)
    val path = finder from s.ant to s.food
    path map (_.compactString) getOrElse ""
  }

  protected def makeScene(str: String): Scene = {

    def find[A <: Positionable](symbol: Char): List[Tile] = {
      for {
        (line, row) <- (str.lines map (_.trim)).zipWithIndex
        (char, col) <- line.zipWithIndex
        if (char == symbol)
      } yield Tile(col, row)
    }.toList

    val game = makeGame(Board(water = find('w'), myAnts = find('a'), enemyAnts = find('e'), food = find('f')))

    Scene(game, game.board.myAnts.values.head, game.board.food.values.head)
  }

  protected def makeGame(board: Board) = Game(
    turn = 42,
    board = board,
    parameters = GameParameters.dummy,
    memory = Memory.dummy,
    vision = Set()
  )

  protected case class Scene(game: Game, ant: MyAnt, food: Food)
}
