package antwar.foundation

final class Timer {

  val start: Long = System.currentTimeMillis

  override def toString = {
    val current = System.currentTimeMillis
    ">   " + (current - start)/ 1000.0 + " s"
  }
}
