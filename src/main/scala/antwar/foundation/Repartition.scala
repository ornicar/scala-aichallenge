package antwar.foundation

case class Repartition(world: World, viewRadius: Int) {

  case class Sector(center: Tile)

  val sectors: List[Sector] = {

      val rowStep = world.rows.toFloat / (world.rows / viewRadius)
      val colStep = world.cols.toFloat / (world.cols / viewRadius)
      val shifts = (List.fill(world.rows)(List(0, rowStep / 2))).flatten

      for {
      (row, index) <- (.0 until world.rows by rowStep).toList.zipWithIndex
      shift = shifts(index).toFloat
      col <- (shift until world.cols + shift by colStep).toList
      tile = Tile(row.toInt, col.toInt)
    } yield Sector(tile)
  }

  def isSectorCenter(tile: Tile) = sectors.map(_.center) contains tile

  override def toString = {
    val chars = world.tiles map { t => if (isSectorCenter(t)) 'x' else '.' }
    ((chars grouped world.cols) map (_ mkString " ")) mkString "\n"
  }
}
