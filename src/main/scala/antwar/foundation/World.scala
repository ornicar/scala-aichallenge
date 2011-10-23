package antwar.foundation

import scala.math.{abs,min,pow,sqrt}

case class World(rows: Int, cols: Int) {

  def distanceFrom(one: Tile) = new {
    def to(other: Tile): Double = {
      val dCol = abs(one.col - other.col)
      val dRow = abs(one.row - other.row)
      min(dCol, cols - dCol) + min(dRow, cols - dRow)
    }
  }

  def directionFrom(one: Tile) = new {
    def to(other: Tile): Set[CardinalPoint] =
      (nsDirection(one, other).toList ::: ewDirection(one, other).toList).toSet
  }

  def nsDirection(one: Tile, other: Tile): Option[CardinalPoint] =
    if (one.row < other.row) {
      if (other.row - one.row >= rows / 2) Some(North) else Some(South)
    } else if (one.row > other.row) {
      if (one.row - other.row >= rows / 2) Some(South) else Some(North)
    } else None

  def ewDirection(one: Tile, other: Tile): Option[CardinalPoint] =
    if (one.col < other.col) {
      if (other.col - one.col >= cols / 2) Some(West) else Some(East)
    } else if (one.col > other.col) {
      if (one.col - other.col >= cols / 2) Some(East) else Some(West)
    } else None

  def singleDirection(one: Tile, other: Tile): Option[CardinalPoint] =
    nsDirection(one, other) orElse ewDirection(one, other)

  def nsDistance(one: Tile, other: Tile): Option[(CardinalPoint, Int)] =
    nsDirection(one, other) map { (_, abs(one.row - other.row)) }

  def ewDistance(one: Tile, other: Tile): Option[(CardinalPoint, Int)] =
    ewDirection(one, other) map { (_, abs(one.col - other.col)) }

  def tile(aim: CardinalPoint) = new {
    def of(tile: Tile) = {
      aim match {
        case North => tile.copy(row = if (tile.row == 0) rows - 1 else tile.row - 1)
        case South => tile.copy(row = (tile.row + 1) % rows)
        case East => tile.copy(col = (tile.col + 1) % cols)
        case West => tile.copy(col = if (tile.col == 0) cols - 1 else tile.col - 1)
      }
    }
  }
}
