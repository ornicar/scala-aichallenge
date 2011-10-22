package antwar

import foundation._

case class PathFinder(game: Game) {

  def world = game.world

  def from(f: Tile) = new {
    def to(t: Tile): Option[Path] = {
      def dirDistanceToList(dirDist: Option[(CardinalPoint, Int)]): List[CardinalPoint] = dirDist match {
        case None => Nil
        case Some((dir, dist)) => List.fill(dist)(dir)
      }
      val aims = dirDistanceToList(world.nsDistance(f, t)) ::: dirDistanceToList(world.ewDistance(f, t))
      println("%d permutations" format aims.permutations.toList.size)
      val validAims = aims.permutations find { aims => Path(world, aims, f).trip forall (game free _) }
      validAims map { Path(world, _, f) }
    }
  }

}
