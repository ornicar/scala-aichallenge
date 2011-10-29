package antwar

import foundation.Logger

class FoodAssigner[Ant] {

  val max = 99

  type AntOptions = List[Option[Ant]]

  def assign(foodsAntsDist: List[Map[Ant, Int]]): AntOptions = {

    val foodsCandidates: List[AntOptions] = foodsAntsDist map { as =>
      (as map (_._1)).toList
    } map { fas =>
      None :: (fas map { Some(_) })
    }

    def assignements(fcs: List[AntOptions]): List[AntOptions] = {
      fcs match {
        case Nil => List(Nil)
        case foodCandidates :: rest => {
          for {
            solution <- assignements(rest)
            candidate <- foodCandidates
            if (candidate.isEmpty || !(solution contains candidate))
          } yield candidate :: solution
        }
      }
    }

    val solutions = assignements(foodsCandidates)

    def cost(solution: AntOptions, dists: List[Map[Ant, Int]]): Int = solution match {
      case Nil => 0
      case ant :: rest => cost(rest, dists.tail) + (ant map dists.head getOrElse max)
    }

    val solution = solutions minBy { cost(_, foodsAntsDist) }

    solution
  }

}
