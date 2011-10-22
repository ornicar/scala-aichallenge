package antwar

import foundation._

sealed trait Path {

  def tiles: List[Tile]

  def aims: List[CardinalPoint]

  def from: Tile

  def to: Tile = tiles.last

  override def toString: String = aims mkString ", "
}

final class TilePath(world: World, val tiles: List[Tile]) extends Path {

  def from = tiles.head

  lazy val aims = ((tiles.head, List[CardinalPoint]()) /: tiles.tail) { case ((prevTile, aims), tile) =>
    (tile, world.singleDirection(prevTile, tile) :: aims)
  }._2.reverse
}

final class AimPath(world: World, val aims: List[CardinalPoint], val from: Tile) extends Path {

  lazy val tiles = (List(from) /: aims) { case (ts, aim) => (world tile aim of ts.head) :: ts }.reverse

}

object Path {

  def apply(world: World, tiles: List[Tile]): TilePath = new TilePath(world, tiles)

  def apply(world: World, aims: List[CardinalPoint], from: Tile): AimPath = new AimPath(world, aims, from)
}
