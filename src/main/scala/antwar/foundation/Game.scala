package antwar.foundation

import antwar.Memory
import scala.math

sealed trait GameLike {
  val parameters: GameParameters
  val memory: Memory
  val knownWater: List[Tile]
}

case class GameSetup(parameters: GameParameters, memory: Memory) extends GameLike {

  val knownWater: List[Tile] = Nil
}

case class Game(
  turn: Int,
  parameters: GameParameters,
  board: Board,
  memory: Memory,
  vision: Set[Tile]) extends GameLike {

  val knownWater: List[Tile] = board.water.keys.toList

  def tiles = parameters.tiles

  lazy val world = World(parameters.rows, parameters.cols)

  def moving(from: Tile, to: Tile): Game = copy(board = board.moving(from, to))

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def visible(tile: Tile): Boolean = vision contains tile
}
