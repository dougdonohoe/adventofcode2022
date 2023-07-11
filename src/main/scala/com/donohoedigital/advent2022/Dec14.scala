package com.donohoedigital.advent2022

import java.awt.geom.Point2D
import scala.collection.mutable

object Const {
  val sandOrigin: CavePoint = CavePoint(500, 0)
}

// https://adventofcode.com/2022/day/14
object Dec14 extends App {

  val input = Input.readFile("day14.txt", trim = false).split("\n")

  val points = input.map(line => {
    line.split(" -> ").map(p => {
      val coords = p.split(",")
      CavePoint(coords(0).toInt, coords(1).toInt)
    })
  })

  val lines = points.flatMap(row =>
    row.sliding(2).map(a => CaveLine(a(0), a(1)))
  )

  // The sand is pouring into the cave from point 500,0.
  //   4     5  5
  //   9     0  0
  //   4     0  3
  // 0 ......+...
  // 1 ..........
  // 2 ..........
  // 3 ..........
  // 4 ....#...##
  // 5 ....#...#.
  // 6 ..###...#.
  // 7 ........#.
  // 8 ........#.
  // 9 #########.

  // Part 1 - correct answer is 964
  val cave = Cave(lines)
  println("Part 1 - total at rest " + cave.addSand(250))

  // Part 2 - correct answer is 32041
  val floor_y = cave.lowestY + 2
  val floor = CaveLine(CavePoint(-10000, floor_y), CavePoint(10000, floor_y))
  val cave2 = Cave((floor :: lines.toList).toArray)
  println("Part 2 - total at rest " + cave2.addSand(5000))
}

case class CavePoint(x: Int, y: Int) {
  def down: CavePoint = copy(y = y + 1)
  def downLeft: CavePoint = copy(x = x - 1, y = y + 1)
  def downRight: CavePoint = copy(x = x + 1, y = y + 1)

  override def toString: String = x + "," + y
}

case class CaveLine(from: CavePoint, to: CavePoint) {
  def contains(p: CavePoint): Boolean = {
    // return distance(A, C) + distance(B, C) == distance(A, B);
    val diff = Point2D.distance(from.x, from.y, p.x, p.y) + Point2D.distance(to.x, to.y, p.x, p.y) - Point2D.distance(to.x, to.y, from.x, from.y)
    math.abs(diff) <= 0.000001d
  }

  override def toString: String = from + " -> " + to
}

case class Cave(lines: Array[CaveLine]) {

  val lowestY: Int = lines.map(x => math.max(x.from.y, x.to.y)).max

  val sand = new mutable.HashMap[CavePoint, Boolean]
  val blockedCache = new mutable.HashMap[CavePoint, Boolean]

  def addSand(reportEvery: Int): Int = {
    var count = 0
    while (addSand(Const.sandOrigin)) {
      //println(cave.sand.size + " at rest: " + cave2.sand.keys.mkString(", "))
      count += 1
      if (count % reportEvery == 0) {
        println(sand.size + " at rest")
      }
    }
    sand.size
  }

  def addSand(point: CavePoint): Boolean = {
    if (isAboveVoid(point)) {
      println("Above void: " + point)
      false
    } else if (!isBlocked(point.down)) {
      addSand(point.down)
    } else if (!isBlocked(point.downLeft)) {
      addSand(point.downLeft)
    } else if (!isBlocked(point.downRight)) {
      addSand(point.downRight)
    } else {
      sand(point) = true
      point != Const.sandOrigin
    }
  }

  def isBlocked(point: CavePoint): Boolean = {
    if (sand.contains(point)) {
      true
    } else {
      anyLineContains(point)
    }
  }

  def anyLineContains(point: CavePoint): Boolean = {
    // cache previous hits for faster results
    if (blockedCache.contains(point)) {
      blockedCache(point)
    } else {
      val blocked = lines.exists(_.contains(point))
      blockedCache(point) = blocked
      blocked
    }
  }

  def isAboveVoid(point: CavePoint): Boolean = {
    val overAnyLine = (point.y + 1 to lowestY).exists(y =>
      anyLineContains(CavePoint(point.x, y))
    )
    !overAnyLine
  }
}
