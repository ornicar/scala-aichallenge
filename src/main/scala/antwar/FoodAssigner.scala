package antwar

object FoodAssigner {

  //(A, 5) (B, 7) (C, 9)
  //(A, 4) (D, 8)
  //(B, 5)
  //(B, 6)

  //C = 9
  //A = 4
  //B = 5
  //-- = 99

  //A, B, C
  //A, D
  //B
  //B

  //A D B -
  //B A - -
  //C A B -
  //C A - B
  //C D B -
  //C D - B

  def assign[Ant](foodsAntsDist: List[Map[Ant, Int]]): List[Ant] = {
    val foodAnts: List[List[Ant]] = foodsAntsDist map (_._1)
    val ants: List[Ant] = foodAnts flatMap identity
  }

}
