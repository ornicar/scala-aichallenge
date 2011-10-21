import scala.util.Random

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)

    def antDirection(ant: MyAnt, occupied: Set[Tile]): Option[CardinalPoint] = {

      def canGo(tile: Tile) = !(game.board.water contains tile) && !(occupied contains tile)

      Random.shuffle(directions) find { dir => canGo(game tile dir of ant.tile) }
    }

    def recursiveOrders(ants: List[MyAnt], occupied: Set[Tile] = Set()): Set[Order] = ants match {
      case Nil => Set()
      case ant :: otherAnts => antDirection(ant, occupied) match {
        case None => recursiveOrders(otherAnts, occupied)
        case Some(dir) => recursiveOrders(otherAnts, occupied + game.tile(dir).of(ant.tile)) + Order(ant.tile, dir)
      }
    }

    recursiveOrders(game.board.myAnts.values.toList)
  }
}
