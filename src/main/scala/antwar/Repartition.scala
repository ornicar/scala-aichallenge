package antwar

import foundation._

import scala.math.{sqrt, min}

case class Repartition(world: World, viewRadius: Int) {

  lazy val centers: List[Tile] = sectors.keys.toList

  def sectorOf(tile: Tile): Sector = {
    sectors.values find { _ contains tile }
  }.get

  // all sectors, the nearest from this tile first
  def nearestSectors(tile: Tile): List[Sector] =
    sectors.values.toList sortBy { s => world.distanceFrom(s.center, tile) }

  def isSectorCenter(tile: Tile) = centers contains tile

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

  override def toString = {
    (0 until world.rows) map { row =>
      (0 until world.cols) map { col =>
        val tile = Tile(row, col)
        val sector = sectorOf(tile)
        val sectorIndex = sectors.values.toSeq indexOf sector
        tile match {
          case p if centers contains p => '@'
          case p if (sectorIndex % 3 == 0) => '+'
          case p if (sectorIndex % 3 == 1) => 'o'
          case p if (sectorIndex % 3 == 2) => '.'
          case p => '!'
        }
      }
    } map (_ mkString " ") mkString "\n"
  }
}

case class Sector(center: Tile, tiles: Set[Tile]) extends Positionable {

  val tile = center

  def contains(tile: Tile) = tiles contains tile

  override def toString = "Sector(%s -> %s)".format(tile, tiles.toList.sorted)
}
