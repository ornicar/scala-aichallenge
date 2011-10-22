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

sealed trait Game extends TileSystem {
  val turn: Int
  val parameters: GameParameters
  val rows: Int = parameters.rows
  val columns: Int = parameters.columns
  val board: Board
  val gameOver: Boolean

  def free(tile: Tile) = !(board.myAnts contains tile) && !(board.water contains tile)

  def choices(tile: Tile) =
    CardinalPoint.all filter { aim => free(this tile aim of tile) }

  def nearest[A <: Positionable](objs: Iterable[A]) = new {
    def from(tile: Tile): Option[A] = Proximity.sorted(objs, tile).headOption map (_.obj)
  }

  private case class Proximity[+A <: Positionable](obj: A, dist: Int)

  private object Proximity {

    def apply[A <: Positionable](obj: A, tile: Tile): Proximity[A] =
      Proximity(obj, (Game.this walkingDistanceFrom obj.tile to tile))

    def sorted[A <: Positionable](objs: Iterable[A], tile: Tile): Seq[Proximity[A]] =
      (objs map { apply(_, tile) }).toSeq sortWith { (a, b) => a.dist < b.dist }
  }
}

