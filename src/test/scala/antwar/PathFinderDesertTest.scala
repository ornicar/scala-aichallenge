package antwar

import foundation._
import org.scalatest._

class PathFinderDesertTest extends PathFinderTest {

  val extendWorld = true

  test("Find straight east path") {
    val path = findPath("""wwwwwww
                           w.a...f
                           wwwwwww""")
    assert(path === "eeee")
  }

  test("Find straight west path") {
    val path = findPath("""wwwwwww
                           .f...aw
                           wwwwwww""")
    assert(path === "wwww")
  }

  test("Find straight north path") {
    val path = findPath("""w.f..w
                           w....w
                           w....w
                           w.a..w
                           wwwwww""")
    assert(path === "nnn")
  }

  test("Find straight south path") {
    val path = findPath("""wwwwww
                           w.a..w
                           w....w
                           w....w
                           w.f..w""")
    assert(path === "sss")
  }

  test("Find north-east diagonal") {
    val path = findPath("""wwwwww
                           w...fw
                           w....w
                           w....w
                           wa...w
                           wwwwww""")
    assert(path === "nnneee")
  }

  test("Find south-west diagonal") {
    val path = findPath("""wwwwww
                           w...aw
                           w....w
                           w....w
                           wf...w
                           wwwwww""")
    assert(path === "ssswww")
  }
}
