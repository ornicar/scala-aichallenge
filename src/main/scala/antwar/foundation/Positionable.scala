package antwar.foundation

sealed trait Positionable {
  val tile: Tile
}
case class MyAnt(tile: Tile) extends Positionable
case class EnemyAnt(tile: Tile) extends Positionable
case class Food(tile: Tile) extends Positionable
case class Water(tile: Tile) extends Positionable
