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
  def aim2(ant: MyAnt, game: Game) = {
    game choices ant match {
      case Nil => None
      case choices => {
        val informedChoices = choices map { aim =>
          val tile = game.world.tile(aim, 1) of ant
          (aim, game.isolation(tile))
        }
        Some(informedChoices.maxBy(_._2)._1)
      }
    }
  }
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val aim = PathFinder(game).search(ant, target) map { _.aims.head }
    aim orElse any(ant, game)
  }

  def target: Tile
}

case class GetFood(food: Food) extends Goto {
  override def target = food.tile
}
