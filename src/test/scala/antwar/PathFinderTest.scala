package antwar

import foundation._

import org.scalatest._

class PathFinderTest extends FunSuite {

  //test("Find straight east path") {
    //val path = findPath("""wwwwww
                           //.a...f
                           //wwwwww""")
    //assert(path === "eeee")
  //}

  //test("Find straight west path") {
    //val path = findPath("""wwwwww
                           //.f...a
                           //wwwwww""")
    //assert(path === "wwww")
  //}

  //test("Find straight north path") {
    //val path = findPath("""w.f..w
                           //w....w
                           //w....w
                           //w.a..w""")
    //assert(path === "nnn")
  //}

  //test("Find straight south path") {
    //val path = findPath("""w.a..w
                           //w....w
                           //w....w
                           //w.f..w""")
    //assert(path === "sss")
  //}

  //test("Find north-east diagonal") {
    //val path = findPath("""w...fw
                           //w....w
                           //w....w
                           //wa...w""")
    //assert(path === "nenene")
  //}

  //private def findPath(str: String): String = {
    //val s = makeScene(str)
    //val finder = PathFinder(s.game)
    //val path = finder from s.ant to s.food
    //finder showPath path
  //}

  //private def makeScene(str: String): Scene = {

    //def find[A <: Positionable](symbol: Char, builder: Tile => A): Map[Tile, A] = {
      //for {
        //(line, row) <- (str.lines map (_.trim)).zipWithIndex
        //(char, col) <- line.zipWithIndex
        //if (char == symbol)
        //tile = Tile(col, row)
      //} yield (tile, builder(tile))
    //}.toMap

    //val water = find('w', tile => Water(tile))
    //val myAnts = find('a', tile => MyAnt(tile))
    //val food = find('f', tile => Food(tile))
    //val game = GameInProgress(board = Board(water = water, myAnts = myAnts, food = food))
    //Scene(game, myAnts.values.head, food.values.head)
  //}

  //case class Scene(game: Game, ant: MyAnt, food: Food)
}
