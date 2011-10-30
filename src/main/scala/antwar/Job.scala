package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def valid(game: Game): Boolean

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]): Option[CardinalPoint] = shuffle(choices).headOption

  def any(ant: MyAnt, game: Game): Option[CardinalPoint] = any(game choices ant)

  def orAny(ant: MyAnt, game: Game, aim: Option[CardinalPoint]): Option[CardinalPoint] =
    aim match {
      case None => any(ant, game)
      case Some(aim) if (game choices ant contains aim) => Some(aim)
      case _ => any(ant, game)
    }
}

case class Explore() extends Job {

  def valid(game: Game) = false

  //def aim(ant: MyAnt, game: Game) = any(ant, game)
  def aim(ant: MyAnt, game: Game) = {
    any(ant, game)
  }
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val aim = new PathFinder(game.world, game.board.water.keySet).search(ant, target) map { _.aims.head }
    orAny(ant, game, aim)
  }

  def target: Tile

  override def toString = "%d, %d".format(target.row, target.col)
}

case class GetFood(target: Tile) extends Goto {

  //def valid(game: Game) = game.board.food contains target
  def valid(game: Game) = false

  override def toString = "Food(%s)" format super.toString
}

case class Patrol(sector: Sector) extends Goto {

  def valid(game: Game) = true

  override def target = sector.center

  override def toString = "Patrol(%s)" format super.toString
}
