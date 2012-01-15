package antwar

import foundation._

import scala.util.Random.shuffle

class Assigner(game: Game) {

  val world = game.world
  val board = game.board
  val ants = board.myAntSet
  val foods = board.foodList
  val pathFinder = new PathFinder(world, board.water.keySet)
  val goalAssigner = new GoalAssigner[MyAnt]

  implicit def assignementsToAnt(as: Set[Assignement]): Set[MyAnt] = as map (_.ant)

  def distribute: List[Assignement] = {

    val persistentFeeders = game.memory.antJobs collect {
      case (ant, job: GetFood) => Assignement(ant, job)
    } toSet

    val freeFoods = foods -- game.memory.antJobs collect {
      case (ant, job: GetFood) => job
    } toSet


    val feeders: Set[Assignement] =
      persistentFeeders ++ electFeeders(ants -- persistentFeeders, foods)

    val persistents: Set[Assignement] = {
      for {
        (ant, job) <- game.memory.antJobs
        if !(feeders map (_.ant) contains ant)
        if job.valid(ant, game)
      } yield Assignement(ant, job)
    }.toSet

    val patrollers: Set[Assignement] = electPatrollers(
      ants -- feeders -- persistents,
      game.repartition)

    Logger(this.getClass)("%d foods, %d ants, %d feeders, %d persistents, %d patrollers".format(foods.size, ants.size, feeders.size, persistents.size, patrollers.size))
    Logger("feeders")(feeders)
    Logger("persistents")(persistents)

    val targets = ((patrollers ++ persistents) map { _.job match {
      case patrol: Patrol => Some(patrol.target)
      case _ => None
    }}).flatten
    Logger("patrol targets")(targets)

    feeders.toList ::: persistents.toList ::: patrollers.toList
  }

  def idle(ants: Set[MyAnt], assignements: Set[Assignement]): Set[MyAnt] =
    ants -- (assignements map (_.ant))

  def electFeeders(ants: Set[MyAnt], foods: List[Food]): Set[Assignement] =
    electMovers(ants, foods) { GetFood(_) }

  def electPatrollers(ants: Set[MyAnt], repartition: Repartition): Set[Assignement] = {
    val assignements = ants map { ant =>
      val choices = for {
        sector <- repartition nearestSectors ant drop 1 take 12
        path <- pathFinder.search(ant, sector)
        frequentation = game sectorOccupation sector
      } yield (sector, frequentation, path)

      if (choices.isEmpty) None
      else {
        val bestChoice = choices minBy { _._2 }
        //Logger("choices")((choices map { c => (c._1.center, c._2) }) + " | " + bestChoice._1.center + " " + bestChoice._2)
        Some(Assignement(ant, (Patrol(bestChoice._1, bestChoice._3))))
      }
    }

    assignements.flatten.toSet
  }

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
    val paths = nearAnts take 3 map { ant =>
      (ant, pathFinder.search(ant, to))
    }
    val distances = paths filterNot (_._2.isEmpty) map {
      case (a, p) => (a, p.get.distance)
    }
    distances.toMap
  }
}

