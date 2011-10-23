package antwar.foundation

case class Timer(label: String) {

  val start: Long = System.currentTimeMillis

  def log = Logger(label)(currentTime + " ms")

  def currentTime = System.currentTimeMillis - start

  override def toString =
    "%d - %s".format(currentTime, label)
}
