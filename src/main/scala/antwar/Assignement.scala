package antwar

import antwar.foundation._

case class Assignement(ant: MyAnt, job: Job) {

  def aim(game: Game): Option[CardinalPoint] = job.aim(ant, game)

  def tile = ant.tile
}

class Assigner(board: Board) {

  def distribute: Set[Assignement] =
    board.myAntSet map { ant =>
      Assignement(ant, Explore())
    }

  //def assignTasks(ants: Set[MyAnt]): Set[Worker] = {
    //val foodGatherers = assignFoodGatherers
  //}

  //def findFoodGatherers(ants: Set[MyAnt], foods: List[Food]): Set[FoodGatherer] = (ants, foods) match {
    //case (ants, _) if ants.isEmpty => Set()
    //case (_, Nil) => Set()
    //case (ants, food :: otherFoods) => {
      //val ant = (game nearest ants from food.tile).get
      //findFoodGatherers(ants - ant, otherFoods) + FoodGatherer(ant, food)
    //}
  //}

  //def findExplorers(ants: Set[MyAnt]): Set[Explorer] =
    //ants map { Explorer(_) }
}
