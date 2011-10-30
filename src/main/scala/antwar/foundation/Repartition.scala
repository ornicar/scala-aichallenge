package antwar.foundation

case class Repartition(world: World, viewRadius: Int) {

  def sectorOf(tile: Tile): Sector =
    sectors(centers minBy { world.distanceFrom(_, tile) })

  // all sectors, the nearest from this tile first
  def nearestSectors(tile: Tile): List[Sector] =
    sectors.values.toList sortBy { s => world.distanceFrom(s.center, tile) }

  val sectors: Map[Tile, Sector] = {

      val rowStep = world.rows.toFloat / (world.rows / viewRadius)
      val colStep = world.cols.toFloat / (world.cols / viewRadius)
      val shifts = (List.fill(world.rows)(List(0, rowStep / 2))).flatten

      val pairs = for {
        (row, index) <- (.0 until world.rows by rowStep).toList.zipWithIndex
        shift = shifts(index).toFloat
        col <- (shift until world.cols + shift by colStep).toList
        tile = Tile(row.toInt, col.toInt)
      } yield (tile, Sector(tile))

      pairs.toMap
  }

  val centers: List[Tile] = sectors.keys.toList

  def isSectorCenter(tile: Tile) = centers contains tile

  override def toString = {
    val chars = world.tiles map { t => if (isSectorCenter(t)) 'x' else '.' }
    ((chars grouped world.cols) map (_ mkString " ")) mkString "\n"
  }
}

case class Sector(center: Tile)
