package antwar.foundation

trait Bot {
  def ordersFrom(gameState: Game): Set[Order]
}
