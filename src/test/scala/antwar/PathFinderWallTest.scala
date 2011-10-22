package antwar

import foundation._
import org.scalatest._

class PathFinderWallTest extends PathFinderTest {

  test("Find north east path, horizontal wall") {
    val path = findPath("""wwwwww
                           .....f
                           ......
                           wwww.w
                           a.....
                           wwwwww""")
    assert(path === "eeeennne")
  }

  test("Find north west path, horizontal wall") {
    val path = findPath("""wwwwww
                           f.....
                           ......
                           .wwwww
                           .....a
                           wwwwww""")
    assert(path === "wwwwwnnn")
  }

  test("Find south west path, horizontal wall") {
    val path = findPath("""wwwwww
                           a.....
                           ......
                           .wwwww
                           .....f
                           wwwwww""")
    assert(path === "ssseeeee")
  }

  test("Find north west path, diagonal walls") {
    val path = findPath("""wwwwww
                           f.....
                           ..w...
                           ...w..
                           ....ww
                           .....a
                           wwwwww""")
    assert(path === "wwnwnwnnw")
  }

}
