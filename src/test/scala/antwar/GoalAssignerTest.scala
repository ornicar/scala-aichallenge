package antwar.test

import antwar._
import foundation._

import org.scalatest._

class GoalAssignerTest extends FunSuite {

  val foodAssigner = new GoalAssigner[String]

  test("Assign only one food to one ant") {
    val foodsAntsDist = List(
      Map("A" -> 5)
    )
    assert(foodAssigner.assign(foodsAntsDist) === List(Some("A")))
  }

  test("Assign only one food to 3 ant") {
    val foodsAntsDist = List(
      Map("A" -> 5, "B" -> 2, "C" -> 9)
    )
    assert(foodAssigner.assign(foodsAntsDist) === List(Some("B")))
  }

  test("Assign 3 foods to 1 ant") {
    val foodsAntsDist = List(
      Map("A" -> 5),
      Map("A" -> 4),
      Map("A" -> 7)
    )
    assert(foodAssigner.assign(foodsAntsDist) === List(None, Some("A"), None))
  }

  test("Assign 4 foods to 4 ants") {
    val foodsAntsDist = List(
      Map("A" -> 5, "B" -> 7, "C" -> 8),
      Map("A" -> 4, "D" -> 8),
      Map("B" -> 5),
      Map("B" -> 6)
    )
    assert(foodAssigner.assign(foodsAntsDist) === List(Some("C"), Some("A"), Some("B"), None))
  }

}
