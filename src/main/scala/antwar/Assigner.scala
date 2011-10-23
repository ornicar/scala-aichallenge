package antwar

import foundation._

class Assigner(game: Game) {

  val world = game.world
  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList
  val pathFinder = PathFinder(game)

  def distribute: List[Assignement] = {
    val t = Timer("assignement")
    val feeders = electFeeders(ants, foods)
    val explorers = electExplorers(idle(ants, feeders))
    println("distribute %d feeders & %d explorers" format (feeders.size, explorers.size))

    val assignements = feeders.toList ::: explorers.toList
    t.print
    assignements
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(ants: Set[MyAnt], foods: List[Food]): Set[Assignement] = (ants, foods) match {
    case (ants, _) if ants.isEmpty => Set()
    case (_, Nil) => Set()
    case (ants, food :: otherFoods) => this nearest ants.toSeq from food match {
      case None => electFeeders(ants, otherFoods)
      case Some(ant) => {
        println("ant %s near from food %s (rest %d foods)" format (ant, food, otherFoods.size))
        electFeeders(ants - ant, otherFoods) + Assignement(ant, GetFood(food))
      }
    }
  }

  def electExplorers(ants: Set[MyAnt]): Set[Assignement] =
    ants map { Assignement(_, Explore()) }

  def nearest[A <: Positionable](objs: Seq[A]) = new {
    def from(tile: Tile): Option[A] = {
      val objPaths: Seq[(A, Path)] = for {
        obj <- objs
        if (world distanceFrom obj to tile) < 14
        path <- pathFinder from obj to tile
      } yield (obj, path)
      objPaths.sortWith{ case (a, b) => a._2.size < b._2.size }.headOption map (_._1)
    }
  }
}
