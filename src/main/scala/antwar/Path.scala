package antwar

import foundation._

sealed trait Path {

  def tiles: List[Tile]

  def aims: List[CardinalPoint]

  def from: Tile

  def size: Int

  def to: Tile = tiles.last

  def trip = tiles.tail.init

  override def toString: String = aims mkString ", "

  def compactString: String = aims map (_.symbol) mkString ""
}

final class TilePath(world: World, val tiles: List[Tile]) extends Path {

  def from = tiles.head

  lazy val aims = ((tiles.head, List[CardinalPoint]()) /: tiles.tail) { case ((prevTile, aims), tile) =>
    (tile, world.singleDirection(prevTile, tile) :: aims)
  }._2.reverse

  lazy val size = aims.size
}

final class AimPath(world: World, val aims: List[CardinalPoint], val from: Tile) extends Path {

  lazy val tiles = (List(from) /: aims) { case (ts, aim) => (world tile aim of ts.head) :: ts }.reverse

  lazy val size = tiles.size - 1
}

object Path {

  def apply(world: World, tiles: List[Tile]): TilePath = new TilePath(world, tiles)

  def apply(world: World, aims: List[CardinalPoint], from: Tile): AimPath = new AimPath(world, aims, from)
}
