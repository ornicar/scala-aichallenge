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

  val world = World(parameters.rows, parameters.cols)

  val fullRepartition: Repartition = Repartition.full(world, parameters.viewRadius)

  val emptyRepartition: Repartition = Repartition.empty(fullRepartition, memory.patrolledSectors)

  def tiles = world.tiles

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def isolation(tile: Tile): Double = {
    (.0 /: board.myAnts.keys) { (a, t) =>
      a + world.distanceFrom(tile, t)
    }
  }

  def saturation(tile: Tile): Int = {
    board.myAnts.keys count { t =>
      world.flyDistanceFrom(tile, t) < 10
    }
  }

  //def isolationDistance(one: Tile, other: Tile): Double = {
    //import scala.math._
    //val dRow = abs(one.row - other.row)
    //val dCol = abs(one.col - other.col)
    //sqrt(pow(dRow, 2) + pow(dCol, 2))
  //}

  def visible(tile: Tile): Boolean = vision contains tile
}
