import antwar.foundation._

package object antwar {

  implicit def positionable2Tile(positionable: Positionable): Tile = positionable.tile

  /**
   * Provides oneliner side effects
   * See http://hacking-scala.posterous.com/side-effecting-without-braces
   */
  implicit def anyToSideEffect[A](any: A) = new {
    def ~(sideEffect: (=>A) => Unit): A = {
      sideEffect(any)
      any
    }
    def ~(title: String): A = {
      Logger(title)(any)
      any
    }
  }
}
