package antwar

import foundation._

object OrnicarBot extends App {
  new AntsGame().run(new OrnicarBot)
}

class OrnicarBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    def recursiveOrders(assignements: List[Assignement], game: Game): Set[Order] = assignements match {
      case Nil => Set()
      case assignement :: rest => {
        assignement aim game match {
          case None => recursiveOrders(rest, game)
          case Some(aim) => {
            val newGame = game.moving(assignement.ant, game.world tile aim of assignement.ant)
            recursiveOrders(rest, newGame) + Order(assignement.ant, aim)
          }
        }
      }
    }

    val assignements = (new Assigner(game)).distribute

    recursiveOrders(assignements, game)
  }
}
