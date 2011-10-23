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

  //def aimTimeout(ant: MyAnt, game: Game) = {
    //val pathFinder = PathFinder(game)
    //val choices = game choices ant
    //val targets: List[(Tile, Int)] = for {
      //tile <- game.memory.unseen.toList filter game.free
      //dist = (game.world distanceFrom ant to tile).toInt
    //} yield (tile, dist)
    //val tiles = targets sortWith { _._2 < _._2 } map (_._1) take 10
    //for {
      //tile <- tiles find { t => (pathFinder from ant to t).isDefined }
      //path <- pathFinder from ant to tile
    //} yield path.aims.head
  //}
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
