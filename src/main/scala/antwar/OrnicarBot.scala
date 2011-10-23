package antwar

import foundation._

object OrnicarBot extends App {
  new AntsGame().run(new OrnicarBot)
}

class OrnicarBot extends Bot {

  def ordersFrom(game: Game): List[Order] = {

    val assignements = Timer.monitor("assign") {
      (new Assigner(game)).distribute
    }

    assignements flatMap { assignement =>
      assignement aim game map { aim =>
        val ant = assignement.ant
        val destination = game.world tile aim of ant
        game.board.move(ant, destination)
        Order(ant, aim)
      }
    }
  }
}
