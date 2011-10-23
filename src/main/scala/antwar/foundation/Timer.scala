package antwar.foundation

case class Timer(label: String) {

  val start: Long = System.currentTimeMillis

  def log() { Logger(label)(currentTime + " ms") }

  def currentTime = System.currentTimeMillis - start

  override def toString = "%d ms".format(currentTime)
}

object Timer {

  def monitor[A](label: String)(op: => A): A = {
    val timer = Timer(label)
    val result = op
    timer.log()
    result
  }
}
