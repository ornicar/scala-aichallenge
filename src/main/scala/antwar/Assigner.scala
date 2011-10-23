package antwar

import foundation._

class Assigner(game: Game) {

  val world = game.world
  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList
  val pathFinder = PathFinder(game)

  def distribute: List[Assignement] = {
    val feeders = electFeeders(ants, foods)
    val explorers = electExplorers(idle(ants, feeders))

    Logger(this.getClass)("%d foods, %d ants, %d feeders, %d explorers".format(foods.size, ants.size, feeders.size, explorers.size))

    feeders.toList ::: explorers.toList
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(myAnts: Set[MyAnt], foods: List[Food]): Set[Assignement] = (myAnts, foods) match {
    case (ants, _) if ants.isEmpty => Set()
    case (_, Nil) => Set()
    case (ants, food :: otherFoods) => this nearest ants.toSeq from food match {
      case None => electFeeders(ants, otherFoods)
      case Some(ant) => {
        electFeeders(ants - ant, otherFoods) + Assignement(ant, GetFood(food))
      }
    }
  }

  def electExplorers(ants: Set[MyAnt]): Set[Assignement] =
    ants map { Assignement(_, Explore()) }

  def nearest(objs: Seq[MyAnt]) = new {
    def from(tile: Tile): Option[MyAnt] = {
      val objPaths: Seq[(MyAnt, Path)] = for {
        obj <- objs
        if (world distanceFrom obj to tile) < 14
        path <- pathFinder from obj to tile
      } yield (obj, path)
      objPaths.sortWith{ case (a, b) => a._2.size < b._2.size }.headOption map (_._1)
    }
  }
}
