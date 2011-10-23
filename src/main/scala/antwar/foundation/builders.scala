package antwar.foundation

import antwar.Memory
import scala.collection.mutable

trait Builder {

  def linesToMap(lines: List[String]): Map[String, String] = {
    val pairs = lines map (_.trim) map { _ span (' '!=) } map { case (k, v) => (k, v.trim) }
    pairs.toMap
  }

  def linesToPairs(lines: List[String]): List[(String, String)] =
    lines map (_.trim) map { _ span (' '!=) } map { case (k, v) => (k, v.trim) }
}

class GameBuilder(parameters: GameParameters) extends Builder {

  // precalculate squares around an ant to set as visible
  val visionOffsets: Seq[(Int, Int)] = {
    import parameters._
    val mx = math.sqrt(viewRadius).toInt
    for {
      dRow <- (-mx to mx+1)
      dCol <- (-mx to mx+1)
      d = math.pow(dRow, 2) + math.pow(dCol, 2)
      if d < viewRadius
    } yield (dRow, dCol)
  }

  val tileRegex = """(\d+)\s(\d+)""".r
  val antRegex = """(\d+)\s(\d+)\s(\d+)""".r

  def apply(lines: List[String], from: GameLike): Option[Game] = {

    if (lines.head == "end") return None

    var turn: Int = 0
    val water = mutable.ListBuffer[Tile]()
    val food = mutable.ListBuffer[Tile]()
    val myAnts = mutable.ListBuffer[Tile]()
    val enemyAnts = mutable.ListBuffer[Tile]()

    def tile(str: String): Tile = str match {
      case tileRegex(row, col) => Tile(col.toInt, row.toInt)
    }

    def ant(str: String): Either[Tile, Tile] = str match {
      case antRegex(row, col, player) => player.toInt match {
        case 0 => Left(Tile(col.toInt, row.toInt))
        case _ => Right(Tile(col.toInt, row.toInt))
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
      case ("h", x) => // TODO hive
      case ("d", x) => // TODO dead
    }

    val board = Board(myAnts.toList, enemyAnts.toList, allWater.toList, food.toList)
    val vision = makeVision(board)
    val memory = from.memory seeing vision

    Some(Game(turn, from.parameters, board, memory, vision))
  }

  // All tiles actually visibles
  private def makeVision(board: Board): Set[Tile] = {
    for {
      Tile(aRow, aCol) <- board.myAnts.keys
      (vRow, vCol) <- visionOffsets
    } yield Tile((aRow + vRow) % parameters.rows, (aCol + vCol) % parameters.cols)
  }.toSet
}

class SetupBuilder extends Builder {

  def apply(lines: List[String]): GameSetup = {

    val m = linesToMap(lines.init) map { case (k, v) => (k, v.toInt) }

    val tiles = (for { row <- (0 to m("rows") -1); col <- (0 to m("cols") -1) } yield Tile(row, col)).toSet

    val parameters = GameParameters(m("loadtime"), m("turntime"), m("rows"), m("cols"), m.get("player_seed") getOrElse(42), m("turns"), m("viewradius2"), m("attackradius2"), m("spawnradius2"), tiles)

    val memory = Memory(tiles)

    GameSetup(parameters, memory)
  }
}
