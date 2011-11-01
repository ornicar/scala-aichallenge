package antwar.test

import antwar._
import foundation._

import org.scalatest._

class RepartitionTest extends FunSuite {

  val (rows, cols, viewRadius) = (29, 34, 55)
  val repartition = Repartition(World(rows, cols), viewRadius)

  test("Find wich sector a tile belongs to") {

    assert(Some(Tile(0,0)) === (repartition sectorOf Tile(0,0) map (_.tile)))
    assert(Some(Tile(0,0)) === (repartition sectorOf Tile(2,2) map (_.tile)))
    assert(Some(Tile(7,3)) === (repartition sectorOf Tile(6,2) map (_.tile)))
    assert(Some(Tile(0,0)) === (repartition sectorOf Tile(28, 33) map (_.tile)))
  }

  test("Find nearest sectors") {

    val fromTile = Tile(3, 3)
    val expected = (Tile(7,3) :: Tile(0, 0) :: Tile(0, 8) :: Nil) map repartition.sectors
    val got = repartition nearestSectors fromTile take 3

    assert(got === expected)
  }

  test("Medium size repartition rendering") {
    val expected = """
x . . . . . . . x . . . . . . . . x . . . . . . . x . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . x . . . . . . . . x . . . . . . . x . . . . . . . . x . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
x . . . . . . . x . . . . . . . . x . . . . . . . x . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . x . . . . . . . . x . . . . . . . x . . . . . . . . x . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ."""
    val got = "\n" + repartition.toString
    assert(got === expected)
  }
}
