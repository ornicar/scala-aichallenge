package antwar

import antwar.foundation._

case class Memory(age: Int, tiles: List[Tile]) {

  def aim: Option[CardinalPoint] = None
}
