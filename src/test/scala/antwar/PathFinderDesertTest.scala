package antwar

import foundation._
import org.scalatest._

class PathFinderDesertTest extends PathFinderTest {

  test("Find straight east path") {
    val path = findPath("""wwwwww
                           .a...f
                           wwwwww""")
    assert(path === "eeee")
  }

  test("Find straight west path") {
    val path = findPath("""wwwwww
                           .f...a
                           wwwwww""")
    assert(path === "wwww")
  }

  test("Find straight north path") {
    val path = findPath("""w.f..w
                           w....w
                           w....w
                           w.a..w""")
    assert(path === "nnn")
  }

  test("Find straight south path") {
    val path = findPath("""w.a..w
                           w....w
                           w....w
                           w.f..w""")
    assert(path === "sss")
  }

  test("Find north-east diagonal") {
    val path = findPath("""w...fw
                           w....w
                           w....w
                           wa...w""")
    assert(path === "nnneee")
  }

  test("Find south-west diagonal") {
    val path = findPath("""w...aw
                           w....w
                           w....w
                           wf...w""")
    assert(path === "ssswww")
  }
}
