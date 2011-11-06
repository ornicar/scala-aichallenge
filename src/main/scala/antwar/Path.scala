package antwar

import foundation._

sealed trait Path {

  def tiles: List[Tile]

  def pathTiles = tiles.tail.init

  def aims: List[CardinalPoint]

  def from: Tile

  def size: Int

  def distance = size - 1

  def to: Tile = tiles.last

  def trip = tiles.tail.init

  def aimFrom(tile: Tile): Option[CardinalPoint] =
      aims.lift.apply(pathTiles indexOf tile)

  override def toString: String = aims mkString ", "

  def compactString: String = aims map (_.symbol) mkString ""
}

final class TilePath(world: World, val tiles: List[Tile]) extends Path {

  Logger("path")(tiles)
  assert(tiles forall world.tileSet.contains)

  def from = tiles.head

  lazy val aims = ((tiles.head, List[CardinalPoint]()) /: tiles.tail) { case ((prevTile, aims), tile) =>
    (tile, world.singleDirection(prevTile, tile).get :: aims)
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
