package antwar

import antwar.foundation._

import scala.util.Random

class Queen(game: GameInProgress) {

  // get orders for all ants
  def orders: Set[Order] = {
    def recursiveOrders(assignements: List[Assignement], game: GameInProgress): Set[Order] = assignements match {
      case Nil => Set()
      case assignement :: rest => {
        val aim = assignement aim game
        aim match {
          case None => recursiveOrders(rest, game)
          case Some(aim) => {
            val newGame = game.moving(assignement.tile, game tile aim of assignement.tile)
            recursiveOrders(rest, newGame) + Order(assignement.tile, aim)
          }
        }
      }
    }
    recursiveOrders(assignements.toList, game)
  }

  def assignements: Set[Assignement] = (new Assigner(game.board)).distribute

  def canGo(tile: Tile, occupied: Set[Tile]) = !(game.board.water contains tile) && !(occupied contains tile)
}
