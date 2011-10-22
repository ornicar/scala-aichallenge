package antwar.foundation

import scala.math.{abs,min,pow}

case class Tile(column: Int, row: Int)

trait TileSystem {

  val rows: Int
  val columns: Int

  def distanceFrom(one: Tile) = new {
    def to(another: Tile): Double = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      pow(min(dRow, rows - dRow), 2) + pow(min(dCol, columns - dCol), 2)
    }
  }

  def walkingDistanceFrom(one: Tile) = new {
    def to(another: Tile): Int = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      dRow + dCol
    }
  }

  def directionFrom(one: Tile) = new {
    def to(other: Tile): Set[CardinalPoint] = {
      val ns: Set[CardinalPoint] = if (one.row < other.row) {
        if (other.row - one.row >= rows / 2) Set(North) else Set(South)
      } else if (one.row > other.row) {
        if (one.row - other.row >= rows / 2) Set(South) else Set(North)
      } else Set()

      val ew: Set[CardinalPoint] = if (one.column < other.column) {
        if (other.column - one.column >= columns / 2) Set(West) else Set(East)
      } else if (one.column > other.column) {
        if (one.column - other.column >= columns / 2) Set(East) else Set(West)
      } else Set()

      ns ++ ew
    }
  }

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
