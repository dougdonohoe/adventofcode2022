package com.donohoedigital.advent2022

import scala.collection.mutable.ListBuffer

object Dec12 extends App {

  val example =
    """Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""".stripMargin

  var lines = Input.readFile("day12.txt", trim = false).split("\n")
  //lines = example.split("\n")

  // Each spot is a single lowercase letter, where a is the lowest elevation, b is the
  // next-lowest, and so on up to the highest elevation, z.
  //
  // Also included on the heightmap are marks for your current position (S) and the
  // location that should get the best signal (E). Your current position (S)
  // has elevation a, and the location that should get the best signal (E) has elevation z.
  //
  // To avoid needing to get out your climbing gear, the elevation of the destination
  // square can be at most one higher than the current square (but can be any lower).

  val points = lines.zipWithIndex.map { case (line, y) =>
    line.toCharArray.zipWithIndex.map { case (value, x) =>
      MapPoint(x, y, value)
    }
  }

  val map = ElfMap(points)
  val s = map.find('S')
  val e = map.find('E')
  println("S: " + s + "  E: " + e)
  println("Width: " + map.width + ", height: " + map.height)

  // Part 1: correct answer is 350
  map.shortestPath(s, e)
  println("Part 1 shortest path: " + map.shortest)

  // Part 2: correct answer is 349
  val lowest = map.findLowest()
  val shortest = lowest.map(mp =>  {
    map.reset()
    map.shortestPath(mp, e)
    map.shortest
  });
  println("Lowest points: " + lowest.mkString(" "))
  println("Shortest paths: " + shortest.mkString(" "))
  println("Shortest overall: " + shortest.min)

}

case class MapPoint(x: Int, y: Int, value: Char) {
  var visited = false
  var shortestToGetHere: Int = Int.MaxValue

  def numValue: Int = {
    if (value == 'E') 'z'
    else if (value == 'S') 'a'
    else value
  }

  override def toString: String = "[" + x + "," + y + " " + value + "]"
}

case class ElfMap(grid: Array[Array[MapPoint]], verbose: Boolean = false) {

  var shortest: Int = Int.MaxValue

  def at(x: Int, y: Int): MapPoint = {
    grid(y)(x)
  }

  def reset(): Unit = {
    shortest = Int.MaxValue
    for (row <- grid) {
      row.foreach(m => {
        m.visited = false
        m.shortestToGetHere = Int.MaxValue
      })
    }
  }

  def shortestPath(from: MapPoint, to: MapPoint, count: Int = 0, path: List[MapPoint] = Nil):Unit = {

    // been here, done that
    if (from.visited) {
      return
    }

    // if I'm at target, track shortest
    if (from.x == to.x && from.y == to.y) {
      if (verbose) println("FOUND NEW PATH: " + count)
      if (count < shortest) {
        if (verbose) println("FOUND NEW SHORT ***** : " + count)
        shortest = count
      }
      return
    }

    // if I've already been to from before, and got here quicker, then stop
    if (count >= from.shortestToGetHere) {
      if (verbose) println("Stopping at " + from + " since got here in " + from.shortestToGetHere + " vs " + count)
      return
    } else {
      from.shortestToGetHere = count
    }

    // update path and count
    val newPath = from :: path
    val newCount = count + 1

    if (verbose) println("From: " + from + " To: " + to + " Count: " + count + " Path: " + newPath.reverse.mkString(" "))

    // mark as having visited so we don't loop through again
    from.visited = true

    // up
    if (from.y > 0) {
      val next = at(from.x, from.y - 1)
      if (next.numValue <= from.numValue + 1) {
        shortestPath(next, to, newCount, newPath)
      }
    }

    // down
    if (from.y < (height - 1)) {
      val next = at(from.x, from.y + 1)
      if (next.numValue <= from.numValue + 1) {
        shortestPath(next, to, newCount, newPath)
      }
    }

    // left
    if (from.x > 0) {
      val next = at(from.x - 1, from.y)
      if (next.numValue <= from.numValue + 1) {
        shortestPath(next, to, newCount, newPath)
      }
    }

    // right
    if (from.x < (width - 1)) {
      val next = at(from.x + 1, from.y)
      if (next.numValue <= from.numValue + 1) {
        shortestPath(next, to, newCount, newPath)
      }
    }

    from.visited = false
  }

  val height: Int = grid.length
  val width: Int = grid(0).length

  def findLowest(): Array[MapPoint] = {
    grid.flatMap(_.find(mp => mp.value == 'a' || mp.value == 'S'))
  }

  def find(c: Char): MapPoint = {
    grid.flatMap(_.find(_.value == c)).head
  }
}
