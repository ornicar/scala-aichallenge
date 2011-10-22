package antwar

import foundation._

trait Memory {

  def unseen: Set[Tile]
}

case class GameMemory(unseen: Set[Tile]) extends Memory {

	def seeing(vision: Set[Tile]): GameMemory = copy(
		unseen = unseen -- vision
	)
}

case class EmptyMemory() extends Memory {

  def unseen = Set[Tile]()
}

object Memory {

  def apply(game: Game): GameMemory = GameMemory(
    unseen = game.tiles.toSet
  )
}
