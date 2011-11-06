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

  // set the existing ants, potentially correcting the previous prediction
	def update(vision: Set[Tile], ants: Set[MyAnt]): Memory = copy(
    unseen = unseen -- vision,
    antJobs = antJobs filter { ants contains _._1 }
  )

  // set the next ants positions with their current job
  def withAntJobs(ajs: Map[MyAnt, Job]): Memory = copy(antJobs = ajs)
}

object Memory {

  def dummy: Memory = Memory(unseen = Set.empty, antJobs = Map.empty)
}
