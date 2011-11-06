package antwar.foundation

trait Positionable {
  val tile: Tile
}
case class MyAnt(tile: Tile) extends Positionable {

  override def toString = "Ant(%d, %d)".format(tile.row, tile.col)
}
case class EnemyAnt(tile: Tile) extends Positionable
case class Food(tile: Tile) extends Positionable
case class Water(tile: Tile) extends Positionable
case class MyHill(tile: Tile) extends Positionable
case class EnemyHill(tile: Tile) extends Positionable
