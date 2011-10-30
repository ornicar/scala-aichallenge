package antwar.foundation

import scala.math.{sqrt, min}

trait Repartition {

  def world: World

  val sectors: Map[Tile, Sector]

  lazy val centers: List[Tile] = sectors.keys.toList

  def sectorOf(tile: Tile): Sector =
    sectors(centers minBy { world.distanceFrom(_, tile) })

  // all sectors, the nearest from this tile first
  def nearestSectors(tile: Tile): List[Sector] =
    sectors.values.toList sortBy { s => world.distanceFrom(s.center, tile) }

  def isSectorCenter(tile: Tile) = centers contains tile

  override def toString = {
    val chars = world.tiles map { t => if (isSectorCenter(t)) 'x' else '.' }
    ((chars grouped world.cols) map (_ mkString " ")) mkString "\n"
  }
}

object Repartition {

  def full(world: World, viewRadius: Int) = FullRepartition(world, viewRadius)

  def empty(full: Repartition, patrolled: Set[Sector]) = EmptyRepartition(
    full.world,
    (full.sectors filterNot {
      case (tile, sector) => patrolled contains sector
    })
  )
}

case class EmptyRepartition(world: World, sectors: Map[Tile, Sector]) extends Repartition

case class FullRepartition(world: World, viewRadius: Int) extends Repartition {

  val sectors: Map[Tile, Sector] = {

      val radius = sqrt(viewRadius).toInt
      val rowStep = min(world.rows, world.rows.toFloat / (world.rows / radius))
      val colStep = min(world.cols, world.cols.toFloat / (world.cols / radius))
      val shifts = (List.fill(world.rows)(List(0, rowStep / 2))).flatten

      val pairs = for {
        (row, index) <- (.0 until world.rows by rowStep).toList.zipWithIndex
        shift = shifts(index).toFloat
        col <- (shift until world.cols + shift by colStep).toList
        tile = Tile(row.toInt, col.toInt)
      } yield (tile, Sector(tile))

      pairs.toMap
  }
}

case class Sector(center: Tile) extends Positionable {

  val tile = center
}
