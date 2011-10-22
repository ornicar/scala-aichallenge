package antwar.foundation.test

import antwar.foundation._

import org.scalatest._

class WorldTest extends FunSuite {

  private val world = World(30, 30)

  test("distanceFrom straight no torus") {
    assert((world distanceFrom Tile(10, 10) to Tile(15, 10)) === 5)
    assert((world distanceFrom Tile(10, 10) to Tile(10, 15)) === 5)
    assert((world distanceFrom Tile(10, 10) to Tile(5, 10)) === 5)
    assert((world distanceFrom Tile(10, 10) to Tile(15, 10)) === 5)
  }

  test("distanceFrom diagonal no torus") {
    assert((world distanceFrom Tile(10, 10) to Tile(15, 15)) === 10)
    assert((world distanceFrom Tile(10, 10) to Tile(5, 5)) === 10)
  }

  test("distanceFrom straight with torus") {
    assert((world distanceFrom Tile(5, 5) to Tile(25, 5)) === 10)
    assert((world distanceFrom Tile(25, 5) to Tile(5, 5)) === 10)
    assert((world distanceFrom Tile(5, 5) to Tile(5, 25)) === 10)
    assert((world distanceFrom Tile(5, 25) to Tile(5, 5)) === 10)
  }

  test("distanceFrom diagonal with torus") {
    assert((world distanceFrom Tile(5, 5) to Tile(25, 25)) === 20)
    assert((world distanceFrom Tile(25, 25) to Tile(5, 5)) === 20)
  }
}
