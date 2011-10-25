package antwar.foundation.test

import antwar.foundation._

import org.scalatest._

class WorldTest extends FunSuite {

  private val world = World(30, 30)

  test("relativeTo no torus") {
    assert(world.relativeTo(Tile(10, 10), Tile(15, 15)) === (5, 5))
    assert(world.relativeTo(Tile(10, 10), Tile(5, 5)) === (-5, -5))
    assert(world.relativeTo(Tile(10, 10), Tile(5, 15)) === (-5, 5))
  }

  test("relativeTo with torus") {
    assert(world.relativeTo(Tile(5, 5), Tile(25, 25)) === (-10, -10))
    assert(world.relativeTo(Tile(25, 25), Tile(5, 5)) === (10, 10))
    assert(world.relativeTo(Tile(5, 25), Tile(25, 5)) === (-10, 10))
  }

  test("flyDistanceFrom straight no torus") {
    assert(world.flyDistanceFrom(Tile(10, 10), Tile(15, 10)) === 5)
    assert(world.flyDistanceFrom(Tile(10, 10), Tile(10, 15)) === 5)
    assert(world.flyDistanceFrom(Tile(10, 10), Tile(5, 10)) === 5)
    assert(world.flyDistanceFrom(Tile(10, 10), Tile(15, 10)) === 5)
  }

  test("flyDistanceFrom straight with torus") {
    assert(world.flyDistanceFrom(Tile(5, 5), Tile(25, 5)) === 10)
    assert(world.flyDistanceFrom(Tile(25, 5), Tile(5, 5)) === 10)
    assert(world.flyDistanceFrom(Tile(5, 5), Tile(5, 25)) === 10)
    assert(world.flyDistanceFrom(Tile(5, 25), Tile(5, 5)) === 10)
  }

  test("flyDistanceFrom diagonal with torus") {
    assert(world.flyDistanceFrom(Tile(2, 2), Tile(28, 29)) === 5)
    assert(world.flyDistanceFrom(Tile(29, 29), Tile(3, 2)) === 5)
  }

  test("distanceFrom straight no torus") {
    assert(world.distanceFrom(Tile(10, 10), Tile(15, 10)) === 5)
    assert(world.distanceFrom(Tile(10, 10), Tile(10, 15)) === 5)
    assert(world.distanceFrom(Tile(10, 10), Tile(5, 10)) === 5)
    assert(world.distanceFrom(Tile(10, 10), Tile(15, 10)) === 5)
  }

  test("distanceFrom diagonal no torus") {
    assert(world.distanceFrom(Tile(10, 10), Tile(15, 15)) === 10)
    assert(world.distanceFrom(Tile(10, 10), Tile(5, 5)) === 10)
  }

  test("distanceFrom straight with torus") {
    assert(world.distanceFrom(Tile(5, 5), Tile(25, 5)) === 10)
    assert(world.distanceFrom(Tile(25, 5), Tile(5, 5)) === 10)
    assert(world.distanceFrom(Tile(5, 5), Tile(5, 25)) === 10)
    assert(world.distanceFrom(Tile(5, 25), Tile(5, 5)) === 10)
  }

  test("distanceFrom diagonal with torus") {
    assert(world.distanceFrom(Tile(5, 5), Tile(25, 25)) === 20)
    assert(world.distanceFrom(Tile(25, 25), Tile(5, 5)) === 20)
  }
}
