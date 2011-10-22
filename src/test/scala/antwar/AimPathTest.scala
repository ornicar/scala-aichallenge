package antwar

import foundation._

import org.scalatest._

class AimPathTest extends FunSuite {

  private val world = World(30, 30)

  test("Build straight north AimPath") {
    val p = Path(world, List(North, North, North), Tile(10, 10))
    assert(p.from === Tile(10, 10))
    assert(p.aims === List(North, North, North))
    assert(p.tiles === ((10 to 7 by -1) map { Tile(10, _) }))
    assert(p.to === Tile(10, 7))
  }

  test("Build straight south AimPath") {
    val p = Path(world, List(South, South, South), Tile(10, 10))
    assert(p.tiles === ((10 to 13) map { Tile(10, _) }))
  }

  test("Build straight west AimPath") {
    val p = Path(world, List(West, West, West), Tile(10, 10))
    assert(p.tiles === ((10 to 7 by -1) map { Tile(_, 10) }))
  }

  test("Build straight east AimPath") {
    val p = Path(world, List(East, East, East), Tile(10, 10))
    assert(p.tiles === ((10 to 13) map { Tile(_, 10) }))
  }

  test("Build round AimPath") {
    val p = Path(world, List(East, North, West, South), Tile(10, 10))
    assert(p.from === p.to)
    assert(p.tiles === List(Tile(10, 10), Tile(11, 10), Tile(11, 9), Tile(10, 9), Tile(10, 10)))
  }
}
