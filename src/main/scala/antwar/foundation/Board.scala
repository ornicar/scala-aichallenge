package antwar.foundation

import scala.collection.mutable

case class Board(myAnts: mutable.Map[Tile, MyAnt],
                 enemyAnts: Map[Tile, EnemyAnt],
                 water: Map[Tile, Water],
                 food: Map[Tile, Food]) {

  lazy val elements = myAnts ++ enemyAnts ++ water ++ food

  lazy val myAntSet = myAnts.values.toSet

  lazy val foodList = food.values.toList

  def moving(from: Tile, to: Tile): Board = copy(
    myAnts = (this.myAnts - from).updated(to, MyAnt(to))
  )

  def move(from: Tile, to: Tile) {
    myAnts -= from
    myAnts += ((to, MyAnt(to)))
  }

}

object Board {

  def apply(myAnts: List[Tile],
            enemyAnts: List[Tile],
            water: List[Tile],
            food: List[Tile]): Board = {

    Board(toMutableMap(toMap(myAnts, t => MyAnt(t))),
          toMap(enemyAnts, t => EnemyAnt(t)),
          toMap(water, t => Water(t)),
          toMap(food, t => Food(t)))
  }

  private def toMap[A <: Positionable](tiles: List[Tile], builder: Tile => A) =
    (tiles map { tile => (tile, builder(tile)) }).toMap

  private def toMutableMap[A, B](x: Map[A, B]): mutable.Map[A, B] = x.map(identity)(scala.collection.breakOut)
}
