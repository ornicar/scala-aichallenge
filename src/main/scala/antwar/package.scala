package ornicar

import antwar.foundation._

package object aichallenge {

  implicit def positionable2Tile(positionable: Positionable): Tile = positionable.tile
}
