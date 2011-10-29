package antwar

import foundation._
import scala.math

case class PathFinder(game: Game) {

  val max = 12

  val rows = game.world.rows
  val cols = game.world.cols

  def search(f: Tile, t: Tile) = {
    if (game.world.distanceFrom(f, t) > max) None
    else {
      val astarWorld = makeWorldAround(f.pos)
      val relativeTo = game.world.relativeTo(f, t)
      val somePath = Astar.search(astarWorld, (0, 0), relativeTo) match {
        case Nil => None
        case solution => Some(makePath(f.pos, solution))
      }
      Logger("path")("%s %s %s".format(f, t, somePath map (_.tiles)))
      somePath
    }
  }

  private def makePath(a: Astar.Pos, solution: List[Astar.Pos]) = {
    val tiles = solution map { pos =>
      Tile((a._1 + pos._1) % rows, (a._2 + pos._2) % cols)
    }
    Path(game.world, Tile(a) :: tiles)
  }

  private def makeWorldAround(a: Astar.Pos): Astar.World = {
    for {
      row <- -max to max
      col <- -max to max
      if (math.abs(row) + math.abs(col) <= max)
      tile = Tile((a._1 + row) % rows, (a._2 + col) % cols)
    } yield ((row, col), !(game free tile))
  }.toMap
}
