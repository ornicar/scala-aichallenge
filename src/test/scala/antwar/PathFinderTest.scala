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

    def find[A <: Positionable](symbol: Char, builder: Tile => A): Map[Tile, A] = {
      for {
        (line, row) <- (str.lines map (_.trim)).zipWithIndex
        (char, col) <- line.zipWithIndex
        if (char == symbol)
        tile = Tile(col, row)
      } yield (tile, builder(tile))
    }.toMap

    val water = find('w', tile => Water(tile))
    val myAnts = find('a', tile => MyAnt(tile))
    val food = find('f', tile => Food(tile))
    val game = GameInProgress(board = Board(water = water, myAnts = myAnts, food = food))
    Scene(game, myAnts.values.head, food.values.head)
  }

  protected case class Scene(game: Game, ant: MyAnt, food: Food)
}
