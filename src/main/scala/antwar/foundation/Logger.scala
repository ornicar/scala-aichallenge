package antwar.foundation

import java.io._

object Logger {

  def info = apply("info")(_)
  def err  = apply("err!")(_)

  def clear { file.delete }

  private val file = new File("log")

  def apply(priority: String)(msg: => Any) {
    withWriter(file) { w =>
      w.write("[%s] %s\n".format(priority.toUpperCase, msg.toString))
    }
  }

  private def withWriter(file: File)(op: FileWriter => Unit) {
    val writer = new FileWriter(file, true)
    try {
      op(writer)
    } catch {
      case e =>
    } finally {
      writer.close
    }
  }

}
