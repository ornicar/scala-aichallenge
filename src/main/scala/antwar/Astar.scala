package antwar

import scala.math
import scala.collection.mutable.PriorityQueue

/**
 * Performance oriented implementation of A*
 * Adapted from the work of Brian Grinstead (http://www.briangrinstead.com/blog/astar-search-algorithm-in-javascript-updated)
 * Usage: val solution = Astar.search(world, start, end)
 */
object Astar {

  type Pos = (Int, Int)
  type World = Map[Pos, Boolean]
  type Heuristic = (Pos, Pos) => Int

  def search(world: World, s: Pos, e: Pos, heuristic: Heuristic = manhattan): List[Pos] = {

    val nodes = world map { case (pos, wall) => (pos, new Node(pos, wall)) }
    val graph = new Graph(nodes.toMap)
    val end = graph nodes e

		var heap = new PriorityQueue[Node]()
		heap enqueue graph.nodes(s)

    while(heap.nonEmpty) {

      // Grab the lowest f(x) to process next. Heap keeps this sorted for us.
      var node = heap.dequeue

      // End case -- result has been found, return the traced path
      if (node == end) return path(node).reverse map (_.pos)

      // Normal case -- move node from open to closed, process each of its neighbors
      node.closed = true

      for (neighbor <- graph.neighbors(node); if (!neighbor.wall && !neighbor.closed)) {

        // g score is the shortest distance from start to current node, we need to check if
        // the path we have arrived at this neighbor is the shortest one we have seen yet
        var gScore = node.g + 1
        var beenVisited = neighbor.visited

        if(!beenVisited || gScore < neighbor.g) {
          // Found an optimal (so far) path to this node. Take score for node to see how good it is.
          neighbor.parent = node
          neighbor.update(gScore, heuristic(neighbor.pos, end.pos))

          if (!beenVisited) // Pushing to heap will put it in proper place based on the 'f' value.
            heap enqueue neighbor
        }
      }
    }

    Nil // No result was found -- empty list signifies failure to find path
  }

  def manhattan(pos1: Pos, pos2: Pos): Int = math.abs(pos2._1 - pos1._1) + math.abs(pos2._2 - pos1._2)

  private def path(node: Node): List[Node] = node.parent match {
    case null => Nil
    case parent => node :: path(parent)
  }

  private class Node(val pos: Pos, val wall: Boolean) extends Ordered[Node] {

    var g = 0 // The total cost of getting to that node (pretty straightforward).
    var h = 0 // The estimated time to reach the finish from the current node.
    var f = 0 // Simply g(x) + h(x). The lower the f(x), the better.
    var visited = false
    var closed = false
    var parent: Node = null

    def row = pos._1
    def col = pos._2

    def update(gg: Int, hh: => Int) {
      g = gg
      if (h == 0) h = hh // only compute this once
      f = g + h
      visited = true
    }

    def compare(that: Node) = that.f compare f
  }

  private class Graph(val nodes: Map[Pos, Node]) {

    def neighbors(node: Node) = List(
        nodes.get(node.row -1, node.col)
      , nodes.get(node.row +1, node.col)
      , nodes.get(node.row, node.col -1)
      , nodes.get(node.row, node.col +1)
    ) filterNot (_.isEmpty) map (_.get)
  }
}
