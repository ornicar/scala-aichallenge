package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]) = shuffle(choices).headOption
}

case class Explore() extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val pathFinder = PathFinder(game)
    val choices = game choices ant
    val targets: List[(Tile, Int)] = for {
      tile <- game.memory.unseen.toList
      dist = (game.world distanceFrom ant to tile).toInt
      if dist < 30
    } yield (tile, dist)
    println(targets)
    println("%d unseen, %d targets" format (game.memory.unseen.toList.size, targets.size))
    val tiles = targets sortWith { _._2 < _._2 } map (_._1)

    val aims = tiles map { t => game.world.singleDirection(ant, t) }
    val aim = aims find { aim => choices contains aim }

    aim
  }
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = PathFinder(game) from ant to target map { _.aims.head }

  def target: Tile
}

case class GetFood(food: Food) extends Goto {
  override def target = food.tile
}
