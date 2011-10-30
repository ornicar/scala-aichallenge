package antwar.test

import antwar._
import foundation._

import org.scalatest._

class RepartitionTest extends FunSuite {

  test("Medium size repartition") {

    val repartition = Repartition(World(29, 34), 7)

    val expected = ""
    val got = "\n" + repartition.toString

    assert(got === expected)
  }
}
