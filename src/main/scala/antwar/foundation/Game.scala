package antwar.foundation

import antwar.Sector
import antwar.Memory
import antwar.Repartition
import scala.math

sealed trait GameLike {
  val const: Const
  val memory: Memory
  val knownWater: List[Tile]
}

case class GameSetup(const: Const, memory: Memory) extends GameLike {

  val knownWater: List[Tile] = Nil
}

case class Game(
  turn: Int,
  const: Const,
  board: Board,
  memory: Memory,
  vision: Set[Tile]) extends GameLike {

  def parameters = const.parameters
  def world = const.world
  def repartition = const.repartition

  val knownWater: List[Tile] = board.water.keys.toList

  def tiles = world.tiles

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def visible(tile: Tile): Boolean = vision contains tile

  def sectorOccupation(sector: Sector) =
    board.myAnts.values count { sector.tiles contains _ }
}

object Game {

  def dummy(board: Board, rows: Int, cols: Int) = {

    val parameters = GameParameters.dummy(rows = rows, cols = cols)
    val world = World(rows, cols)
    val repartition = Repartition(world, parameters.viewRadius)
    val const = Const(parameters, world, repartition)

    Game(
      turn = 42,
      board = board,
      const = const,
      memory = Memory.dummy,
      vision = Set()
    )
  }
}
