package antwar.foundation

import antwar.Memory
import scala.math

sealed trait GameLike {
  val parameters: GameParameters
  val memory: Memory
}

case class GameSetup(parameters: GameParameters, memory: Memory) extends GameLike

case class Game(turn: Int, parameters: GameParameters, board: Board, memory: Memory) extends GameLike {

  Logger("create game")(turn)

  lazy val world = World(parameters.rows, parameters.cols)

  lazy val tiles: List[Tile] = {
    for {
      row <- (0 to parameters.rows -1)
      col <- (0 to parameters.cols -1)
    } yield Tile(row, col)
  }.toList

  def moving(from: Tile, to: Tile): Game = copy(board = board.moving(from, to))

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def visible(tile: Tile): Boolean = vision contains tile

  // All tiles actually visibles
  lazy val vision: Set[Tile] = {
    Logger.info("process vision")
    import parameters._
    // precalculate squares around an ant to set as visible
    val mx = math.sqrt(viewRadius).toInt
    val offsets = for {
      dRow <- (-mx to mx+1)
      dCol <- (-mx to mx+1)
      d = math.pow(dRow, 2) + math.pow(dCol, 2)
      if d < viewRadius
    } yield (dRow, dCol)

    val tiles = for {
      Tile(aRow, aCol) <- board.myAnts.keys
      (vRow, vCol) <- offsets
    } yield Tile((aRow + vRow) % rows, (aCol + vCol) % cols)

    tiles.toSet
  }
}
