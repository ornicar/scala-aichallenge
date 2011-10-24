package antwar

import scala.math
import scala.collection.mutable.PriorityQueue

/**
 * Adapted from the work of Brian Grinstead (http://www.briangrinstead.com/blog/astar-search-algorithm-in-javascript-updated)
 */
class Astar(world: Map[(Int, Int), Boolean]) {

  type Pos = (Int, Int)

  type Heuristic = (Pos, Pos) => Int

  class Node(val pos: Pos, val wall: Boolean) extends Ordered[Node] {

    // The total cost of getting to that node (pretty straightforward).
    var g = 0

    // The estimated time to reach the finish from the current node.
    var h = 0

    // Simply g(x) + h(x). The lower the f(x), the better.
    def f = g + h

    var visited = false
    var closed = false
    var parent: Option[Node] = None

    def row = pos._1
    def col = pos._2

    def compare(that: Node) = f compare that.f

    def valid: Boolean = !(wall || closed)
  }

  class Graph(nodes: Map[Pos, Node]) {

    def apply(pos: Pos) = nodes.apply(pos)

    def get(pos: Pos) = nodes.get(pos)

    def neighbors(node: Node) = List(
        get(node.row -1, node.col)
      , get(node.row +1, node.col)
      , get(node.row, node.col -1)
      , get(node.row, node.col +1)
    ) filterNot (_.isEmpty) map (_.get)
  }

  def search(s: Pos, e: Pos, heuristic: Heuristic = manhattan): List[Pos] = {

    val nodes = world map { case (pos, wall) => (pos, new Node(pos, wall)) }
    val graph = new Graph(nodes.toMap)
    val start = graph(s)
    val end = graph(e)

		var openHeap = new PriorityQueue[Node]()

		openHeap.enqueue(start)

    while(openHeap.nonEmpty) {

      // Grab the lowest f(x) to process next.  Heap keeps this sorted for us.
      var currentNode = openHeap.dequeue

      // End case -- result has been found, return the traced path
      if (currentNode == end) return path(currentNode).reverse map (_.pos)

      // Normal case -- move currentNode from open to closed, process each of its neighbors
      currentNode.closed = true

      graph.neighbors(currentNode) filter (_.valid) foreach { neighbor =>

        // g score is the shortest distance from start to current node, we need to check if
        //   the path we have arrived at this neighbor is the shortest one we have seen yet
        // 1 is the distance from a node to it's neighbor.  This could be variable for weighted paths.
        var gScore = currentNode.g + 1
        var beenVisited = neighbor.visited

        if(!beenVisited || gScore < neighbor.g) {

          // Found an optimal (so far) path to this node.  Take score for node to see how good it is.
          neighbor.visited = true
          neighbor.parent = Some(currentNode)
          neighbor.h = if (neighbor.h > 0) neighbor.h else heuristic(neighbor.pos, end.pos)
          neighbor.g = gScore

          if (!beenVisited) // Pushing to heap will put it in proper place based on the 'f' value.
            openHeap.enqueue(neighbor)
          //else // Already seen the node, but since it has been rescored we need to reorder it in the heap
            //openHeap.rescoreElement(neighbor)
        }
      }
    }

    // No result was found -- empty list signifies failure to find path
    Nil
  }

  private def path(node: Node): List[Node] = node.parent match {
    case None => Nil
    case Some(parent) => node :: path(parent)
  }

  def manhattan(pos1: Pos, pos2: Pos): Int =
    math.abs(pos2._1 - pos1._1) + math.abs(pos2._2 - pos1._2)
}
