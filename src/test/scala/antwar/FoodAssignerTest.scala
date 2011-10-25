package antwar.test

import antwar._
import foundation._

import org.scalatest._

class FoodAssignerTest extends FunSuite {

  //f1 (A, 5) (B, 7) (C, 9)
  //f2 (A, 4) (D, 8)
  //f3 (B, 5)
  //f4 (B, 6)

  //f1 C
  //f2 A
  //f3 B
  //f4 --

  test("Assign best food ants") {
    val foodsAntsDist = List(
      Map("A" -> 5, "B" -> 7, "C" -> 9),
      Map("A" -> 4, "D" -> 8),
      Map("B" -> 5),
      Map("B" -> 6)
    )
    assert(FoodAssigner.assign(foodsAntsDist) === List(Some("C"), Some("A"), Some("B"), None))
  }

}
