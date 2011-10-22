package antwar

import foundation._

case class PathFinder(game: Game) {

  def world = game.world

  def from(f: Tile) = new {
    def to(t: Tile): Path = {
      val dRow = t.row - f.row
      val dCol = t.column - f.column
      def dirDistanceToList(dirDist: Option[(CardinalPoint, Int)]): List[CardinalPoint] = dirDist match {
        case None => Nil
        case Some((dir, dist)) => List.fill(dist)(dir)
      }
      val aims = dirDistanceToList(world.nsDistance(f, t)) ::: dirDistanceToList(world.ewDistance(f, t))
      //val directPaths = directPath.permutations
      Path(world, aims, f)
    }
  }

}
