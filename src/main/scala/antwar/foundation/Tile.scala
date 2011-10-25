package antwar.foundation

case class Tile(row: Int, col: Int) {

  def pos = (row, col)
}

object Tile {

  def apply(pos: (Int, Int)): Tile = Tile(pos._1, pos._2)
}
