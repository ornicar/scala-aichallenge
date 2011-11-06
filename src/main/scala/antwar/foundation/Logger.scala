package antwar.foundation

import java.io._

object Logger {

  val enabled = true

  def t(a: Any) = apply("test")(a)

  def info = apply("info")(_)
  def err  = apply("err!")(_)

  def clear { file.delete }

  private lazy val file = new File("log")

  def apply(title: Any)(msg: => Any) {
    if (enabled) {
      withWriter(file) { w =>
        w.write("[%s] %s\n".format(title, msg))
      }
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
