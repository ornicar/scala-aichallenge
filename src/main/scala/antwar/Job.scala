package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]): Option[CardinalPoint] = shuffle(choices).headOption

  def any(ant: MyAnt, game: Game): Option[CardinalPoint] = any(game choices ant)
}

case class Explore() extends Job {

  def aim(ant: MyAnt, game: Game) = any(ant, game)
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val aim = PathFinder(game) from ant to target map { _.aims.head }
    aim orElse any(ant, game)
  }

  def target: Tile
}

case class GetFood(food: Food) extends Goto {
  override def target = food.tile
}
