package antwar.foundation

case class Order(tile: Tile, point: CardinalPoint) {

  def inServerSpeak = "o %d %d %s\n".format(tile.row, tile.col, point.symbol)

}
