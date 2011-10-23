package antwar

import foundation._

class Queen(game: Game) {

  // get orders for all ants
  def orders: Set[Order] = {
    def recursiveOrders(assignements: List[Assignement], game: Game): Set[Order] = assignements match {
      case Nil => Set()
      case assignement :: rest => {
        val aim = assignement aim game
        aim match {
          case None => recursiveOrders(rest, game)
          case Some(aim) => {
            val newGame = game.moving(assignement.ant, game.world tile aim of assignement.ant)
            recursiveOrders(rest, newGame) + Order(assignement.ant, aim)
          }
        }
      }
    }
    recursiveOrders(assignements, game)
  }

  def assignements: List[Assignement] = (new Assigner(game)).distribute

  def canGo(tile: Tile, occupied: Set[Tile]) = !(game.board.water contains tile) && !(occupied contains tile)
}
