package antwar.foundation

import antwar._

import annotation.tailrec
import io.Source
import java.io._

class AntsGame(in: InputStream = System.in, out: OutputStream = System.out) {

  val source = new BufferedSource(in, Source.DefaultBufSize)
  val writer = new BufferedWriter(new OutputStreamWriter(out))

  def run(bot: Bot) = {
    try {

      def playNextTurn(game: Game): Unit = {
        Parser.parse(source, game.parameters, game.board.water, game.memory) match {
          case g: GameOver => Unit
          case g: GameInProgress => {
            val timer = Timer("all")
            // first time, we have to replace the empty memory with a game memory
            val memory = g.memory match {
              case _: EmptyMemory => Memory(g)
              case x: GameMemory => x
            }
            // apply game vision to game memory
            val game = g.copy(memory = memory seeing g.vision)
            val orders = bot.ordersFrom(game)
            orders.map(_.inServerSpeak).foreach(writer.write)
            timer.print
            writer.write("go\n")
            writer.flush
            playNextTurn(game)
          }
        }
      }
      playNextTurn(GameInProgress())

    } catch {
      case t => t.printStackTrace
    }
  }

  def log(file: String = "/tmp/antwarslog")(op: PrintWriter => Unit) {
    val p = new PrintWriter(new File(file))
    try { op(p) } finally { p.close() }
  }
}
