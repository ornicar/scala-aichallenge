package antwar.foundation

import scala.math.{sqrt, pow}

case class GameParameters(loadTime: Int,
                          turnTime: Int,
                          rows: Int,
                          cols: Int,
                          seed: Int,
                          turns: Int,
                          viewRadius: Int,
                          attackRadius: Int,
                          spawnRadius: Int) {

  // precalculate squares around an ant to set as visible
  val visionOffsets: Seq[(Int, Int)] = GameParameters.visionOffsets(rows, cols, viewRadius)
}

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

  def visionOffsets(rows: Int, cols: Int, viewRadius: Int): Seq[(Int, Int)] = {
    val mx = sqrt(viewRadius).toInt
    for {
      dRow <- (-mx to mx+1)
      dCol <- (-mx to mx+1)
      d = pow(dRow, 2) + pow(dCol, 2)
      if d < viewRadius
    } yield (dRow, dCol)
  }
}
