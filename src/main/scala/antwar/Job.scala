package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]): Option[CardinalPoint] = shuffle(choices).headOption

  def any(ant: MyAnt, game: Game): Option[CardinalPoint] = any(game choices ant)
}

case class Explore() extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val choices = game choices ant
    if (choices.isEmpty) None
    else {
      val isolatedChoices = choices map { aim =>
        val tile = game.world tile aim of ant
        (aim, game.isolation(tile))
      }
      val bestIsolatedChoice = isolatedChoices maxBy { _._2 }
      Some(bestIsolatedChoice._1)
    }
  }
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
