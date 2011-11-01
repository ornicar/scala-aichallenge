package antwar

import foundation._
import scala.util.Random.shuffle

trait Job {

  def valid(ant: MyAnt, game: Game): Boolean

  def aim(ant: MyAnt, game: Game): Option[CardinalPoint]

  def any(choices: List[CardinalPoint]): Option[CardinalPoint] = shuffle(choices).headOption

  def any(ant: MyAnt, game: Game): Option[CardinalPoint] = any(game choices ant)

  def orAny(ant: MyAnt, game: Game, aim: Option[CardinalPoint]): Option[CardinalPoint] =
    aim match {
      case None => any(ant, game)
      case Some(aim) if (game choices ant contains aim) => Some(aim)
      case _ => any(ant, game)
    }

  def orNone(ant: MyAnt, game: Game, aim: Option[CardinalPoint]): Option[CardinalPoint] =
    aim flatMap { a =>
      if (game choices ant contains a) Some(a) else None
    }
}

case class Explore() extends Job {

  def valid(ant: MyAnt, game: Game) = false

  //def aim(ant: MyAnt, game: Game) = any(ant, game)
  def aim(ant: MyAnt, game: Game) = {
    any(ant, game)
  }
}

trait Goto extends Job {

  def aim(ant: MyAnt, game: Game) = {
    val aim = path(ant, target, game) map { _.aims.head }
    orAny(ant, game, aim)
  }

  def path(ant: MyAnt, target: Tile, game: Game): Option[Path] =
    new PathFinder(game.world, game.board.water.keySet).search(ant, target)

  def target: Tile

  override def toString = "%d, %d".format(target.row, target.col)
}

case class GetFood(target: Tile) extends Goto {

  //def valid(game: Game) = game.board.food contains target
  def valid(ant: MyAnt, game: Game) = false

  override def toString = "Food(%s)" format super.toString
}

case class Patrol(sector: Sector, fullPath: Path) extends Goto {

  def valid(ant: MyAnt, game: Game) = (game.repartition sectorOf ant) != Some(sector)

  override def target = sector.center

  override def aim(ant: MyAnt, game: Game) =
    orAny(ant, game, fullPath aimFrom ant)

  override def toString = "Patrol(%s)" format super.toString
}
