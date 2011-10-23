package antwar.foundation

import java.io._

object Logger {

  def info = apply("info")(_)
  def err  = apply("err!")(_)

  def clear { file.delete }

  private val file = new File("log")

  def apply(title: Any)(msg: => Any) {
    withWriter(file) { w =>
      w.write("[%s] %s\n".format(title, msg))
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
