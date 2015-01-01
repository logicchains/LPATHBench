object LPath extends App {
  def readPlaces:Array[Node] = {
    val lines = scala.io.Source.fromFile("agraph").getLines().drop(1)
    val linesOfInts = lines.map(_.split(" ").map(_.toInt))
    val linesById = linesOfInts.toSeq.groupBy(_.apply(0))

    linesById.map { lines =>
      val (nodeId, nodeLines) = lines

      val neighbors = nodeLines.map { neighborLine =>
        neighborLine(1) -> neighborLine(2)
      }.toMap

      Node(nodeId, neighbors)
    }.toArray.sortBy(_.id)
  }

  val nodes = readPlaces

  case class Neighbor(id:Int, neighbor:Node, cost:Int)
  case class Node(id:Int, neighborIds:Map[Int,Int]) {
    lazy val neighbors = neighborIds.map(n => Neighbor(n._1, nodes(n._1),n._2)).toArray
  }

  @inline def max(a:Int,b:Int) = if (a >= b) a else b

  def longestPath(nextNode:Node, visited:Array[Boolean], dist:Int):Int = {
    val nnid = nextNode.id
    val ns = nextNode.neighbors
    var m = 0
    var i = 0
    val icount = ns.length

    visited(nnid) = true

    while(i < icount) {
      val n = ns(i)

      if(!visited(n.id)) {
        m = max(m,n.cost + longestPath(n.neighbor, visited, dist))
      }

      i = i + 1
    }

    visited(nnid) = false

    m
  }


  def longestPath_functional(nextNode:Node, visited:scala.collection.immutable.BitSet, dist:Int):Int = {
    nextNode.neighbors.foldLeft(0) { 
      case (lastMax, Neighbor(id,node,_)) if visited.contains(node.id) =>
        max(lastMax,dist)
      case (lastMax, Neighbor(id,node,cost))  =>
        max(lastMax,longestPath_functional(node,visited + node.id,dist + cost))
    }
  }

  //warm up
  for(i <- 1 to 10) {
    longestPath(nodes(0), new Array[Boolean](nodes.size), 0)
  }

  val start = System.nanoTime()
  val len = longestPath(nodes(0), new Array[Boolean](nodes.size), 0)
  val duration = (System.nanoTime() - start) / 1000000
  println(s"$len LANGUAGE Scala $duration")
}
