package antwar

import foundation._
import scala.math

class PathFinder(world: World, water: Set[Tile]) {

  val max = 14

  val rows = world.rows
  val cols = world.cols

  def search(f: Tile, t: Tile) = {
    if (world.distanceFrom(f, t) > max) None
    else {
      val astarWorld = makeWorldAround(f.pos)
      val relativeTo = world.relativeTo(f, t)
      val somePath = Astar.search(astarWorld, (0, 0), relativeTo) match {
        case Nil => None
        case solution => Some(makePath(f.pos, solution))
      }
      //Logger("path")("%s %s %s".format(f, t, somePath map (_.tiles)))
      somePath
    }
  }

  private def makePath(a: Astar.Pos, solution: List[Astar.Pos]) = {
    val tiles = solution map { pos =>
      Tile((a._1 + pos._1) % rows, (a._2 + pos._2) % cols)
    }
    Path(world, Tile(a) :: tiles)
  }

  private def makeWorldAround(a: Astar.Pos): Astar.World = {
    for {
      row <- -max to max
      col <- -max to max
      if (math.abs(row) + math.abs(col) <= max)
      tile = Tile((a._1 + row) % rows, (a._2 + col) % cols)
    } yield ((row, col), water contains tile)
  }.toMap
}
