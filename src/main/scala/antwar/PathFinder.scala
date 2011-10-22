package antwar

import foundation._

case class PathFinder(game: Game) extends TileSystem {

  val columns: Int = game.columns
  val rows: Int = game.rows

  def from(f: Tile) = new {
    def to(t: Tile): Path = {
      Path(Seq())
    }
  }

  def showPath(path: Path): String = path.tiles match {
    case Nil => ""
    case tiles => ((tiles.head, "") /: tiles.tail) { case ((prev, str), t) =>
      (t, (this directionFrom prev to t).head.symbol + str)
    }._2
  }

}

case class Path(tiles: Seq[Tile]) {

  val size = tiles.size
}
