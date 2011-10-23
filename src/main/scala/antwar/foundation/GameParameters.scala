package antwar.foundation

case class GameParameters(loadTime: Int,
                          turnTime: Int,
                          rows: Int,
                          cols: Int,
                          seed: Int,
                          turns: Int,
                          viewRadius: Int,
                          attackRadius: Int,
                          spawnRadius: Int,
                          tiles: Set[Tile])
