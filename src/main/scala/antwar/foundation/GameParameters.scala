package antwar.foundation

case class GameParameters(loadTime: Int,
                          turnTime: Int,
                          rows: Int,
                          cols: Int,
                          seed: Int,
                          turns: Int,
                          viewRadius: Int,
                          attackRadius: Int,
                          spawnRadius: Int)

object GameParameters {

  def dummy(rows: Int = 50, cols: Int = 50): GameParameters = {

    GameParameters(loadTime = 2000,
                    turnTime = 1000,
                    rows = rows,
                    cols = cols,
                    seed = 42,
                    turns = 200,
                    viewRadius = 55,
                    attackRadius = 5,
                    spawnRadius = 1)
  }
}
