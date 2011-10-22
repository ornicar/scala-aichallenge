import antwar.foundation._

package object antwar {

  implicit def positionable2Tile(positionable: Positionable): Tile = positionable.tile
}
