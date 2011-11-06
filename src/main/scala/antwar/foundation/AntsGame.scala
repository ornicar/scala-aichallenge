package antwar.foundation

import antwar._

import io.Source
import java.io._
import annotation.tailrec

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = new BufferedSource(in, Source.DefaultBufSize)
  val writer = new BufferedWriter(new OutputStreamWriter(out))
  val parser = new Parser(source)

  Logger.clear

  def run(bot: Bot) = {

    Logger.info("Running")

    val timer = Timer("setup")
    val setup = (new SetupBuilder)(parser.setup)
    val builder = new GameBuilder(setup.const)
    writeGo
    timer.log()

    @tailrec
    def turn(gameLike: GameLike, memory: Memory) {
      val parsed = parser.turn
      val timer = Timer("total")
      val game = Timer.monitor("build") { builder(parsed, gameLike, memory) }
      game match {
        case None =>
        case Some(game) => {
          val (orders, memory) = Timer.monitor("AI") { bot ordersFrom game  }
          writeOrders(orders)
          Logger.info("------------------------------------------------------- %d | %s".format(game.turn,  timer))
          turn(game, memory)
        }
      }
    }

    try {
      turn(setup, setup.memory)
    } catch {
      case e => logException(e)
    }

    Logger.info("Graceful shutdown.")
  }

  private def logException(e: Throwable) {
    val sw = new StringWriter()
    e.printStackTrace(new PrintWriter(sw));
    Logger("EXCEPTION")(sw.toString)
  }

  private def writeGo = writeOrders(Nil)

  private def writeOrders(orders: List[Order]) {
    val text = orders map (_.inServerSpeak) mkString "\n" + "\n"
    writer write (text + "go\n")
    writer.flush
  }
}
