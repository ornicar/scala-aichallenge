package antwar.foundation

case class Tile(row: Int, col: Int) extends Ordered[Tile] {

  def pos = (row, col)

  def compare(that: Tile) = order compare that.order

  private def order = row * 1000 + col
}

object Tile {

  def apply(pos: (Int, Int)): Tile = Tile(pos._1, pos._2)
}
