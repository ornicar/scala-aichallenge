package antwar.foundation

import antwar.{Memory, EmptyMemory}

import scala.math

case class GameInProgress(turn: Int = 0, parameters: GameParameters = GameParameters(), board: Board = Board(), memory: Memory = EmptyMemory()) extends Game {
  val gameOver = false
  def including[P <: Positionable](positionable: P) = this.copy(board = this.board including positionable)
  def including(p: Positionable*): GameInProgress = p.foldLeft(this){(game, positionable) => game.including(positionable)}

  def moving(from: Tile, to: Tile): GameInProgress = copy(board = board.moving(from, to))
}
case class GameOver(turn: Int, parameters: GameParameters, board: Board, memory: Memory) extends Game {
  val gameOver = true
}

sealed trait Game {
  val turn: Int
  val parameters: GameParameters
  val board: Board
  val gameOver: Boolean
  val memory: Memory
  val world = World(parameters.rows, parameters.columns)

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }

  def tiles: List[Tile] = {
    for {
      row <- (0 to parameters.rows -1)
      col <- (0 to parameters.columns -1)
    } yield Tile(row, col)
  }.toList

  def visible(tile: Tile): Boolean = vision contains tile

  // All tiles actually visibles
  lazy val vision: Set[Tile] = {
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
    } yield Tile((aRow + vRow) % rows, (aCol + vCol) % columns)

    tiles.toSet
  }
}
