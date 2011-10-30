package antwar

import foundation.Logger

import scala.math.pow

class GoalAssigner[Ant] {

  val max = 99

  type AntOptions = List[Option[Ant]]

  def assign(goalsAntsDist: List[Map[Ant, Int]]): AntOptions = {

    val goalsCandidates: List[AntOptions] = goalsAntsDist map { as =>
      (as map (_._1)).toList
    } map { fas =>
      None :: (fas map { Some(_) })
    }

    def assignements(fcs: List[AntOptions]): List[AntOptions] = {
      fcs match {
        case Nil => List(Nil)
        case goalCandidates :: rest => {
          for {
            solution <- assignements(rest)
            candidate <- goalCandidates
            if (candidate.isEmpty || !(solution contains candidate))
          } yield candidate :: solution
        }
      }
    }

    val solutions = assignements(goalsCandidates)

    def cost(solution: AntOptions, dists: List[Map[Ant, Int]]): Int = solution match {
      case Nil => 0
      case ant :: rest => cost(rest, dists.tail) + pow((ant map dists.head getOrElse max).toInt, 2).toInt
    }

    val solution = solutions minBy { cost(_, goalsAntsDist) }

    solution
  }

}
