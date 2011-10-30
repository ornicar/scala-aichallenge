package antwar

import foundation._

case class Memory(
  unseen: Set[Tile],
  antJobs: Map[MyAnt, Job],
  paths: Map[(Tile, Tile), Option[Path]] = Map.empty
) {

  lazy val patrolledSectors: Set[Sector] = {
    antJobs collect {
      case (ant, job: Patrol) => job.sector
    }
  }.toSet

	def seeing(vision: Set[Tile]): Memory = copy(unseen = unseen -- vision)

  // set the next ants positions with their current job
  def withAntJobs(ajs: Map[MyAnt, Job]): Memory = copy(antJobs = ajs)

  // set the existing ants, potentially correcting the previous prediction
  def withAnts(ants: Set[MyAnt]): Memory = copy(
    antJobs = antJobs filter { ants contains _._1 }
  )
}

object Memory {

  def dummy: Memory = Memory(unseen = Set.empty, antJobs = Map.empty)
}
