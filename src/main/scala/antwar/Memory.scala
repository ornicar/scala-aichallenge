package antwar

import foundation._

case class Memory(unseen: Set[Tile]) {

	def seeing(vision: Set[Tile]): Memory = copy(
		unseen = unseen -- vision
	)
}
