package antwar

import foundation._

class Assigner(game: Game) {

  val world = game.world
  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList
  val pathFinder = PathFinder(game)
  val foodAssigner = new FoodAssigner[MyAnt]

  def distribute: List[Assignement] = {
    val feeders = electFeeders(ants, foods)
    val explorers = electExplorers(idle(ants, feeders))

    Logger(this.getClass)("%d foods, %d ants, %d feeders, %d explorers".format(foods.size, ants.size, feeders.size, explorers.size))

    feeders.toList ::: explorers.toList
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(myAnts: Set[MyAnt], foods: List[Food]): Set[Assignement] = {

    val foodsAntDists: List[Map[MyAnt, Int]] = foods map { nearestAnts(myAnts, _) }

    val assignements = foods zip foodAssigner.assign(foodsAntDists) map {
      case (food, None) => None
      case (food, Some(ant)) => Some(Assignement(ant, GetFood(food)))
    }

    (assignements filterNot (_.isEmpty) map (_.get)).toSet
  }

  def electExplorers(ants: Set[MyAnt]): Set[Assignement] =
    ants map { Assignement(_, Explore()) }

  def nearestAnts(ants: Set[MyAnt], to: Tile): Map[MyAnt, Int] = {
    (ants.toList sortBy { -world.distanceFrom(_, to) } take 10 map { ant =>
      (ant, pathFinder.search(ant, to))
    } filterNot (_._2.isEmpty) map { case (a, p) => (a, p.get.distance) }).toMap
  }
}

