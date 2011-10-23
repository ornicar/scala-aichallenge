package antwar.foundation

import antwar.Memory
import scala.collection.mutable

class GameBuilder {

  def makeGame(lines: List[String], from: GameLike): Option[Game] = {

    if (lines.head == "end") return None

    var turn: Int = 0
    val water = mutable.ListBuffer[Tile]()
    val food = mutable.ListBuffer[Tile]()
    val myAnts = mutable.ListBuffer[Tile]()
    val enemyAnts = mutable.ListBuffer[Tile]()

    val m = linesToMap(lines.init)

    val tileRegex = """(\d+)\s(\d+)""".r
    def tile(str: String): Tile = str match {
      case tileRegex(row, col) => Tile(col.toInt, row.toInt)
    }

    val antRegex = """(\d+)\s(\d+)\s(\d+)""".r
    def ant(str: String): Either[Tile, Tile] = str match {
      case antRegex(row, col, player) => player.toInt match {
        case 0 => Left(Tile(col.toInt, row.toInt))
        case _ => Right(Tile(col.toInt, row.toInt))
      }
    }

    m foreach {
      case ("turn", x) => turn = x.toInt
      case ("f", x) => food += tile(x)
      case ("w", x) => water += tile(x)
      case ("a", x) => ant(x) match {
        case Left(a) => myAnts += a
        case Right(a) => enemyAnts += a
      }
      case ("h", x) => // TODO hive
      case ("d", x) => // TODO dead
    }

    assert(turn > 0)

    val board = Board(myAnts, enemyAnts, water, food)

    Some(Game(turn, from.parameters, board, from.memory))
  }

  def makeGameSetup(lines: List[String]): GameSetup = {
    val m = linesToMap(lines.init) map { case (k, v) => (k, v.toInt) }
    val parameters = GameParameters(m("loadtime"), m("turntime"), m("rows"), m("cols"), m.get("player_seed") getOrElse(42), m("turns"), m("viewradius2"), m("attackradius2"), m("spawnradius2"))
    val tiles = (for { row <- (0 to parameters.rows -1); col <- (0 to parameters.cols -1) } yield Tile(row, col)).toSet
    val memory = Memory(tiles)
    GameSetup(parameters, memory)
  }

  private def linesToMap(lines: List[String]): Map[String, String] = {
    val pairs = lines map (_.trim) map { _ span (' '!=) } map { case (k, v) => (k, v.trim) }
    pairs.toMap
  }
}
