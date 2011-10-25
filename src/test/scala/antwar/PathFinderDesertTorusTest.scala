package antwar.test

import antwar._
import foundation._
import org.scalatest._

class PathFinderDesertTorusTest extends PathFinderTest {

  val extendWorld = false

  test("Find straight west path") {
    val path = findPath("""wwwwwww
                           ..a...f
                           wwwwwww""")
    assert(path === "www")
  }

  test("Find straight east path") {
    val path = findPath("""wwwwwww
                           .f...a.
                           wwwwwww""")
    assert(path === "eee")
  }

  test("Find straight south path") {
    val path = findPath("""w.f..w
                           w....w
                           w....w
                           w.a..w
                           w....w""")
    assert(path === "ss")
  }

  test("Find straight north path") {
    val path = findPath("""w....w
                           w.a..w
                           w....w
                           w....w
                           w.f..w""")
    assert(path === "nn")
  }

  test("Find north-east diagonal") {
    val path = findPath("""......
                           .....a
                           ......
                           ......
                           .f....
                           ......""")
    assert(path === "nnnee")
  }

  test("Find south-west diagonal") {
    val path = findPath("""......
                           .....f
                           ......
                           ......
                           .a....
                           ......""")
    assert(path === "sssww")
  }
}
