package antwar.foundation

import antwar.Memory
import antwar.Repartition
import scala.collection.mutable
import scala.math.{sqrt, pow}

trait Builder {

  def linesToMap(lines: List[String]): Map[String, String] = {
    val pairs = lines map (_.trim) map { _ span (' '!=) } map { case (k, v) => (k, v.trim) }
    pairs.toMap
  }

  def linesToPairs(lines: List[String]): List[(String, String)] =
    lines map (_.trim) map { _ span (' '!=) } map { case (k, v) => (k, v.trim) }
}

class GameBuilder(const: Const) extends Builder {

  val tileRegex = """(\d+)\s(\d+)""".r
  val ownRegex = """(\d+)\s(\d+)\s(\d+)""".r

  def apply(lines: List[String], from: GameLike, memory: Memory): Option[Game] = {

    if (lines.head == "end") return None

    var turn: Int = 0
    val water, food, myAnts, enemyAnts, myHills, enemyHills = mutable.ListBuffer[Tile]()

    def tile(str: String): Tile = str match {
      case tileRegex(row, col) => Tile(row.toInt, col.toInt)
    }

    def ant(str: String): Either[Tile, Tile] = str match {
      case ownRegex(row, col, player) => player.toInt match {
        case 0 => Left(Tile(row.toInt, col.toInt))
        case _ => Right(Tile(row.toInt, col.toInt))
      }
    }

    def hill(str: String): Either[Tile, Tile] = str match {
      case ownRegex(row, col, player) => player.toInt match {
        case 0 => Left(Tile(row.toInt, col.toInt))
        case _ => Right(Tile(row.toInt, col.toInt))
      }
    }

    linesToPairs(lines.init) foreach {
      case ("turn", x) => turn = x.toInt
      case ("f", x) => food += tile(x)
      case ("w", x) => water += tile(x)
      case ("a", x) => ant(x) match {
        case Left(a) => myAnts += a
        case Right(a) => enemyAnts += a
      }
      case ("h", x) => hill(x).fold(myHills += _, enemyHills += _)
      case ("d", x) => // TODO dead
    }

    val parameters = const.parameters
    val allWater = water.toList ::: from.knownWater
    val board = Board(myAnts.toList, enemyAnts.toList, allWater, food.toList, myHills.toList, enemyHills.toList)
    val vision = {
      for {
        Tile(aRow, aCol) <- board.myAnts.keys
        (vRow, vCol) <- parameters.visionOffsets
      } yield Tile((aRow + vRow) % parameters.rows, (aCol + vCol) % parameters.cols)
    }.toSet
    val newMemory = memory seeing vision withAnts board.myAnts.values.toSet

    Some(Game(turn, from.const, board, memory, vision))
  }
}

class SetupBuilder extends Builder {

  def apply(lines: List[String]): GameSetup = {

    val m = linesToMap(lines.init) map { case (k, v) => (k, v.toInt) }

    val tiles = (for { row <- (0 to m("rows") -1); col <- (0 to m("cols") -1) } yield Tile(row, col)).toSet

    val parameters = GameParameters(m("loadtime"), m("turntime"), m("rows"), m("cols"), m.get("player_seed") getOrElse(42), m("turns"), m("viewradius2"), m("attackradius2"), m("spawnradius2"))

    val memory = Memory(tiles, Map.empty)
    val world = World(m("rows"), m("cols"))
    val repartition = Repartition(world, parameters.viewRadius)
    val const = Const(parameters, world, repartition)

    GameSetup(const, memory)
  }
}
