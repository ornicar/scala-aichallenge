package antwar.foundation

case class Board(myAnts: Map[Tile, MyAnt],
                 enemyAnts: Map[Tile, EnemyAnt],
                 water: Map[Tile, Water],
                 food: Map[Tile, Food]) {

  lazy val elements = myAnts ++ enemyAnts ++ water ++ food

  lazy val myAntSet = myAnts.values.toSet
  lazy val foodList = food.values.toList

  def moving(from: Tile, to: Tile): Board = copy(
    myAnts = this.myAnts.filterNot{case (t, _) => t == from}.updated(to, MyAnt(to))
  )
}

object Board {

  def apply(myAnts: Seq[Tile],
            enemyAnts: Seq[Tile],
            water: Seq[Tile],
            food: Seq[Tile]): Board = {

    Board(toMap(myAnts, t => MyAnt(t)),
          toMap(enemyAnts, t => EnemyAnt(t)),
          toMap(water, t => Water(t)),
          toMap(food, t => Food(t)))
  }

  private def toMap[A <: Positionable](seq: Seq[Tile], builder: Tile => A) =
    (seq map { tile => (tile, builder(tile)) }).toMap
}
