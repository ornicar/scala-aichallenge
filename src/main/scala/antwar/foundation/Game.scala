package antwar.foundation

import antwar.Repartition
import antwar.Sector
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

  val world = World(parameters.rows, parameters.cols)

  val repartition: Repartition = Repartition(world, parameters.viewRadius)

  def tiles = world.tiles

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def visible(tile: Tile): Boolean = vision contains tile

  def sectorOccupation(sector: Sector) =
    board.myAnts.values count { sector.tiles contains _ }
}
