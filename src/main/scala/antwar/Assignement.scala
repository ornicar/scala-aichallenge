package antwar

import foundation._

case class Assignement(ant: MyAnt, job: Job) {

  def aim(game: Game): Option[CardinalPoint] = job.aim(ant, game)

  override def toString = ant.toString + "->" + job.toString
}
