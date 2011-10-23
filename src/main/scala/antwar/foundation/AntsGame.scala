package antwar.foundation

import antwar._

import io.Source
import java.io._
import annotation.tailrec

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = new BufferedSource(in, Source.DefaultBufSize)
  val writer = new BufferedWriter(new OutputStreamWriter(out))
  val parser = new Parser(source)
  val builder = new GameBuilder

  Logger.clear

  def run(bot: Bot) = {

    Logger.info("Running")

    @tailrec
    def turn(game: Game) {
      val t = Timer("ALL")
      val orders = bot ordersFrom game
      t.log
      writeOrders(orders)
      builder.makeGame(parser.parseTurn, game) match {
        case None =>
        case Some(g) => turn(g)
      }
    }

    try {
      val setup = builder.makeGameSetup(parser.parseSetup)
      writeGo
      builder.makeGame(parser.parseTurn, setup) map turn
    } catch { case e => logException(e) }

    Logger.info("Graceful shutdown.")
  }

  private def logException(e: Throwable) {
    val sw = new StringWriter()
    e.printStackTrace(new PrintWriter(sw));
    Logger("EXCEPTION")(sw.toString)
  }

  private def writeGo = writeOrders(Set.empty)

  private def writeOrders(orders: Set[Order]) {
    orders map (_.inServerSpeak) foreach writer.write
    writer write "go\n"
    writer.flush
  }
}
