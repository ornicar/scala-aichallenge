package antwar

import foundation._

import scala.math.{sqrt, min}

case class Repartition(world: World, viewRadius: Int) {

  lazy val centers: List[Tile] = sectors.keys.toList

  def sectorOf(tile: Tile): Option[Sector] =
    sectors.values find { _ contains tile }

  // all sectors, the nearest from this tile first
  def nearestSectors(tile: Tile): List[Sector] =
    sectors.values.toList sortBy { s => world.distanceFrom(s.center, tile) }

  def isSectorCenter(tile: Tile) = centers contains tile

  override def toString = {
    val chars = world.tiles map { t => if (isSectorCenter(t)) 'x' else '.' }
    ((chars grouped world.cols) map (_ mkString " ")) mkString "\n"
  }

  val sectors: Map[Tile, Sector] = {

    val radius = sqrt(viewRadius).toInt
    val rowStep = min(world.rows, world.rows.toFloat / (world.rows / radius))
    val colStep = min(world.cols, world.cols.toFloat / (world.cols / radius))
    val shifts = (List.fill(world.rows)(List(0, rowStep / 2))).flatten

    val centers: List[Tile] = {
      for {
        (row, index) <- (.0 until world.rows by rowStep).toList.zipWithIndex
        shift = shifts(index).toFloat
        col <- (shift until world.cols + shift by colStep).toList
      } yield Tile(row.toInt % world.rows, col.toInt % world.cols)
    }.distinct

    val tilesClosestCenter: Map[Tile, Tile] = {
      world.tiles map { tile =>
        (tile, centers minBy { world.distanceFrom(_, tile) })
      }
    }.toMap

    val centersArea: Map[Tile, Set[Tile]] =
      tilesClosestCenter groupBy { _._2 } mapValues { _.keySet }

    centersArea map {
      case (center, tiles) => (center, Sector(center, tiles))
    }
  }
}

case class Sector(center: Tile, tiles: Set[Tile] = Set.empty) extends Positionable {

  val tile = center

  def contains(tile: Tile) = tiles contains tile

  override def toString = "Sector(%s -> %s)".format(tile, tiles.toList.sorted)
}
