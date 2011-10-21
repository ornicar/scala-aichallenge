package antwar

import antwar.foundation._
import scala.util.Random.shuffle

trait Job {

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]
}

case class Explore() extends Job {

  def aim(ant: MyAnt, game: Game) = any(game choices ant.tile)

  def any(choices: List[CardinalPoint]) = shuffle(choices).headOption
}

//trait Goto extends Job {
  //def target: Tile
//}

//case class GetFood(food: Food) extends Goto {
  //override def target = food.tile
//}
