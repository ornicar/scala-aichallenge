package antwar.foundation

case class Timer(label: String) {

  val start: Long = System.currentTimeMillis

  def print = println(toString)

  override def toString =
    "TIMER [ %d ] %s".format((System.currentTimeMillis - start), label)
}
