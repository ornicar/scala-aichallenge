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

  def electFeeders(myAnts: Set[MyAnt], foods: List[Food]): Set[Assignement] = {
    Set.empty
    //val foodNearestAnts: List[List[(MyAnt, Int)]] = food map nearest
    //val selectedAnts: List[Ant] = {
      //for {
        //foodAntDistances <- foodNearestAnts
        //(ant, distance) <- foodAntDistances
      //} yield ant
    //}.unique
    //val antsFood: List[(MyAnt, Food)] = for {
      //ant <- selectedAnts
      //food = foods minBy (f =>
  }

  def electExplorers(ants: Set[MyAnt]): Set[Assignement] =
    ants map { Assignement(_, Explore()) }

  def nearestAnts(ants: List[MyAnt], to: Tile): List[(MyAnt, Int)] = {
    ants sortBy { -world.distanceFrom(_, to) } take 8 map { ant =>
      (ant, pathFinder.search(ant, to))
    } filterNot (_._2.isEmpty) map { case (a, p) => (a, p.get.distance) }
  }
}

