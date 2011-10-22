package antwar.foundation

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
  val world = World(parameters.rows, parameters.columns)

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(world tile aim of tile) }
}
