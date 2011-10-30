package antwar

import foundation._

import scala.collection.mutable

object OrnicarBot extends App {
  new AntsGame().run(new OrnicarBot)
}

class OrnicarBot extends Bot {

  def ordersFrom(game: Game): (List[Order], Memory) = {

    val assignements = Timer.monitor("assign") {
      (new Assigner(game)).distribute
    }

    val antJobs = mutable.Map[MyAnt, Job]()

    val orders = assignements flatMap { assignement =>
      assignement aim game map { aim =>
        val ant = assignement.ant
        val destination = game.world tile aim of ant
        game.board.move(ant, destination)
        antJobs += ((MyAnt(destination), assignement.job))
        Order(ant, aim)
      }
    }

    val memory = game.memory withAntJobs antJobs.toMap

    (orders, memory)
  }
}
