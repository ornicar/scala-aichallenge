package antwar

import foundation._

import org.scalatest._

class TilePathTest extends FunSuite {

  private val world = World(30, 30)

  test("Build straight north TilePath") {
    val p = Path(world, ((10 to 7 by -1).toList map { Tile(10, _) }))
    assert(p.from === Tile(10, 10))
    assert(p.aims === List(North, North, North))
    assert(p.tiles === ((10 to 7 by -1) map { Tile(10, _) }))
    assert(p.to === Tile(10, 7))
  }

  test("Build straight south TilePath") {
    val p = Path(world, ((10 to 13).toList map { Tile(10, _) }))
    assert(p.aims === List(South, South, South))
    assert(p.to === Tile(10, 13))
  }

  test("Build straight east TilePath") {
    val p = Path(world, ((10 to 13).toList map { Tile(_, 10) }))
    assert(p.aims === List(East, East, East))
    assert(p.to === Tile(13, 10))
  }

  test("Build round TilePath") {
    val p = Path(world, List(Tile(10, 10), Tile(11, 10), Tile(11, 9), Tile(10, 9), Tile(10, 10)))
    assert(p.aims === List(East, North, West, South))
    assert(p.to === Tile(10, 10))
  }
}
