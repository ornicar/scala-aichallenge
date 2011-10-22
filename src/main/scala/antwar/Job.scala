package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]) = shuffle(choices).headOption
}

case class Explore() extends Job {

  def aim(ant: MyAnt, game: Game) = any(game choices ant.tile)
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = (game.world directionFrom ant to target).head match {
    case aim if game choices ant contains aim => Some(aim)
    case _ => any(game choices ant)
  }

  def target: Tile
}

case class GetFood(food: Food) extends Goto {
  override def target = food.tile
}
