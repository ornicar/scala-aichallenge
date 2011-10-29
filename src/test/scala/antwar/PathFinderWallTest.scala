package antwar.test

import antwar._
import foundation._
import org.scalatest._

class PathFinderWallTest extends PathFinderTest {

  val extendWorld = true

  test("Find north east path, horizontal wall") {
    val path = findPath("""wwwwww
                           .....f
                           ......
                           wwww.w
                           a.....
                           wwwwww""")
    assert(path === "eeeeennn")
  }

  test("Find north west path, horizontal wall") {
    val path = findPath("""wwwwww
                           f.....
                           ......
                           .wwwww
                           .....a
                           wwwwww""")
    assert(path === "nnnwwwww")
  }

  test("Find south west path, horizontal wall") {
    val path = findPath("""wwwwww
                           a.....
                           ......
                           .wwwww
                           .....f
                           wwwwww""")
    assert(path === "eeeeesss")
  }

  test("Find north west path, diagonal walls") {
    val path = findPath("""wwwwww
                           f.....
                           ..w...
                           ...w..
                           ....ww
                           .....a
                           wwwwww""")
    assert(path === "nnnnwwwww")
  }

  test("Find no path north west") {
    val path = findPath("""wwwwwww
                           wf.....w
                           w..w...w
                           w.w.w..w
                           ww...www
                           w.....aw
                           wwwwwwww""")
    assert(path === "")
  }

  test("Find no path south east") {
    val path = findPath("""wwwwwwww
                           wa.....w
                           w..w...w
                           w.w.w..w
                           ww...www
                           w.....fw
                           wwwwwwww""")
    assert(path === "")
  }

}
