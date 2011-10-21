package antwar

import antwar.foundation._

object OrnicarBot extends App {
  new AntsGame().run(new OrnicarBot)
}

class OrnicarBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = game match {
    case g: GameInProgress => (new Queen(g)).orders
    case _ => sys.error("Game finished. Bummer.")
  }
}
