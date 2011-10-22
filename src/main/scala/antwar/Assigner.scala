package antwar

import foundation._

class Assigner(game: Game) {

  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList

  def distribute: List[Assignement] = {
    val feeders = electFeeders(ants, foods)
    val explorers = electExplorers(idle(ants, feeders))
    println("distribute %d feeders & %d explorers" format (feeders.size, explorers.size))

    feeders.toList ::: explorers.toList
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(ants: Set[MyAnt], foods: List[Food]): Set[Assignement] = (ants, foods) match {
    case (ants, _) if ants.isEmpty => Set()
    case (_, Nil) => Set()
    case (ants, food :: otherFoods) => {
      val ant = (game nearest ants from food).get
      println("ant %s near from food %s (rest %d foods)" format (ant, food, otherFoods.size))
      electFeeders(ants - ant, otherFoods) + Assignement(ant, GetFood(food))
    }
  }

  def electExplorers(ants: Set[MyAnt]): Set[Assignement] =
    ants map { Assignement(_, Explore()) }
}
