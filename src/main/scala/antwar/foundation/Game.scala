package antwar.foundation

import scala.math.{abs,min,pow}

case class GameInProgress(turn: Int = 0, parameters: GameParameters = GameParameters(), board: Board = Board()) extends Game {
  val gameOver = false
  def including[P <: Positionable](positionable: P) = this.copy(board = this.board including positionable)
  def including(p: Positionable*): GameInProgress = p.foldLeft(this){(game, positionable) => game.including(positionable)}

  def moving(from: Tile, to: Tile): GameInProgress = copy(board = board.moving(from, to))
}
case class GameOver(turn: Int = 0, parameters: GameParameters = GameParameters(), board: Board = Board()) extends Game {
  val gameOver = true
}

sealed trait Game {
  val turn: Int
  val parameters: GameParameters
  val board: Board
  val gameOver: Boolean

  def distanceFrom(one: Tile) = new {
    def to(another: Tile): Int = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      dRow + dCol
    }
  }

  def directionFrom(one: Tile) = new {
    def to(other: Tile): Set[CardinalPoint] = {
      val ns: Set[CardinalPoint] = if (one.row < other.row) {
        if (other.row - one.row >= parameters.rows / 2) Set(North) else Set(South)
      } else if (one.row > other.row) {
        if (one.row - other.row >= parameters.rows / 2) Set(South) else Set(North)
      } else Set()

      val ew: Set[CardinalPoint] = if (one.column < other.column) {
        if (other.column - one.column >= parameters.columns / 2) Set(West) else Set(East)
      } else if (one.column > other.column) {
        if (one.column - other.column >= parameters.columns / 2) Set(East) else Set(West)
      } else Set()

      ns ++ ew
    }
  }

  def tile(aim: CardinalPoint) = new {
    def of(tile: Tile) = {
      aim match {
        case North => tile.copy(row = if (tile.row == 0) parameters.rows - 1 else tile.row - 1)
        case South => tile.copy(row = (tile.row + 1) % parameters.rows)
        case East => tile.copy(column = (tile.column + 1) % parameters.columns)
        case West => tile.copy(column = if (tile.column == 0) parameters.columns - 1 else tile.column - 1)
      }
    }
  }

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(this tile aim of tile) }

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def nearest[A <: Positionable](objs: Iterable[A]) = new {
    def from(tile: Tile): Option[A] = Proximity.sorted(objs, tile).headOption map (_.obj)
  }

  private case class Proximity[+A <: Positionable](obj: A, dist: Int)

  private object Proximity {

    def apply[A <: Positionable](obj: A, tile: Tile): Proximity[A] =
      Proximity(obj, (Game.this distanceFrom obj.tile to tile))

    def sorted[A <: Positionable](objs: Iterable[A], tile: Tile): Seq[Proximity[A]] =
      (objs map { apply(_, tile) }).toSeq sortWith { (a, b) => a.dist < b.dist }
  }
}

