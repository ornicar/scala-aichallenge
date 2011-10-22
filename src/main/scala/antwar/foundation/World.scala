package antwar.foundation

import scala.math.{abs,min,pow,sqrt}

case class World(rows: Int, columns: Int) {

  def distanceFrom(one: Tile) = new {
    def to(other: Tile): Double = {
      val dCol = abs(one.column - other.column)
      val dRow = abs(one.row - other.row)
      min(dCol, columns - dCol) + min(dRow, columns - dRow)
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
    if (one.column < other.column) {
      if (other.column - one.column >= columns / 2) Some(West) else Some(East)
    } else if (one.column > other.column) {
      if (one.column - other.column >= columns / 2) Some(East) else Some(West)
    } else None

  def singleDirection(one: Tile, other: Tile): CardinalPoint =
    nsDirection(one, other) getOrElse ewDirection(one, other).get

  def nsDistance(one: Tile, other: Tile): Option[(CardinalPoint, Int)] =
    nsDirection(one, other) map { (_, abs(one.row - other.row)) }

  def ewDistance(one: Tile, other: Tile): Option[(CardinalPoint, Int)] =
    ewDirection(one, other) map { (_, abs(one.column - other.column)) }

  def tile(aim: CardinalPoint) = new {
    def of(tile: Tile) = {
      aim match {
        case North => tile.copy(row = if (tile.row == 0) rows - 1 else tile.row - 1)
        case South => tile.copy(row = (tile.row + 1) % rows)
        case East => tile.copy(column = (tile.column + 1) % columns)
        case West => tile.copy(column = if (tile.column == 0) columns - 1 else tile.column - 1)
      }
    }
  }
}
