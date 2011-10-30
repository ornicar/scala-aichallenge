package antwar

import foundation._

class Assigner(game: Game) {

  val world = game.world
  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList
  val pathFinder = new PathFinder(world, board.water.keySet)
  val goalAssigner = new GoalAssigner[MyAnt]

  def distribute: List[Assignement] = {

    val feeders: Set[Assignement] = electFeeders(ants, foods)

    val persistents: Set[Assignement] = {
      for {
        (ant, job) <- game.memory.antJobs
        if !(feeders map (_.ant) contains ant)
        if job valid game
      } yield Assignement(ant, job)
    }.toSet

    val patrollers: Set[Assignement] = electPatrollers(
      idle(ants, feeders ++ persistents),
      game.emptyRepartition
    )

    Logger(this.getClass)("%d foods, %d ants, %d feeders, %d persistents, %d patrollers".format(foods.size, ants.size, feeders.size, persistents.size, patrollers.size))

    feeders.toList ::: persistents.toList ::: patrollers.toList
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(ants: Set[MyAnt], foods: List[Food]): Set[Assignement] =
    electMovers(ants, foods) { GetFood(_) }

  def electPatrollers(ants: Set[MyAnt], repartition: Repartition): Set[Assignement] =
    electMovers(ants, repartition.sectors.values.toList) { Patrol(_) }

  def electMovers[A <: Positionable, B <: Job]
    (ants: Set[MyAnt], goals: List[A])(builder: A => B) = {

    val goalsAntDists: List[Map[MyAnt, Int]] = goals map { goal =>
      nearestAnts(ants, goal)
    }

    val assignements = goals zip goalAssigner.assign(goalsAntDists) map {
      case (goal, None) => None
      case (goal, Some(ant)) => Some(Assignement(ant, builder(goal)))
    }

    assignements.flatten.toSet
  }

  def nearestAnts(ants: Set[MyAnt], to: Tile): Map[MyAnt, Int] = {
    val nearAnts = ants.toList sortBy { world.distanceFrom(_, to) }
    val paths = nearAnts take 2 map { ant =>
      (ant, pathFinder.search(ant, to))
    }
    val distances = paths filterNot (_._2.isEmpty) map {
      case (a, p) => (a, p.get.distance)
    }
    distances.toMap
  }
}

