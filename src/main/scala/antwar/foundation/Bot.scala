package antwar.foundation

import antwar.Memory

trait Bot {

  def ordersFrom(game: Game): List[Order]
}
