package antwar.foundation

import antwar.Repartition

// What won't change during the game
case class Const(
  parameters: GameParameters,
  world: World,
  repartition: Repartition
)
