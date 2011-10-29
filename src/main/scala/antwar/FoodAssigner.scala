package antwar

import scala.math.max

object FoodAssigner {

  //(A, 5) (B, 7) (C, 9)
  //(A, 4) (D, 8)
  //(B, 5)
  //(B, 6)

  //C = 9
  //A = 4
  //B = 5
  //-- = 99

  //A, B, C, -
  //A, D, -
  //B, -
  //B, -

  //A D B -
  //B A - -
  //C A B -
  //C A - B
  //C D B -
  //C D - B

  def assign[Ant](foodsAntsDist: List[Map[Ant, Int]]): List[Option[Ant]] = {
    val foodsAnts: List[List[Ant]] = foodsAntsDist map { as => (as map (_._1)).toList }
    val foodsAntOptions: List[List[Option[Ant]]] = foodsAnts map { fas =>
      None :: (fas map { Some(_) })
    }

    def assignements(fas: List[List[Option[Ant]]]): List[List[Option[Ant]]] = {
      fas match {
        case Nil => List(Nil)
        case foodAnts :: rest => {
          for {
            solution <- assignements(rest)
            candidate <- foodAnts
            if (candidate.isEmpty || !(solution contains candidate))
          } yield candidate :: solution
        }
      }
    }
    val solutions = assignements(foodsAntOptions)

    //val visual = (solutions map { _ map {
      //case None => "-"
      //case Some(x) => x.toString
    //} mkString "" }) mkString "\n"
    //println(visual)

    def cost(solution: List[Option[Ant]], dists: List[Map[Ant, Int]]): Int = solution match {
      case Nil => 0
      case ant :: rest => cost(rest, dists.tail) + (ant match {
        case None =>  999
        case Some(a) => dists.head(a)
      })
    }

    solutions minBy { cost(_, foodsAntsDist) }
  }

}
