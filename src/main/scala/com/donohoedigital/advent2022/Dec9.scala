package com.donohoedigital.advent2022

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class HeadMove(direction: String, count: Int)

case class Point(x: Long, y: Long) {
  def up(num: Int): Point = Point(x, y + num)

  def down(num: Int): Point = Point(x, y - num)

  def left(num: Int): Point = Point(x - num, y)

  def right(num: Int): Point = Point(x + num, y)

  def moveTowards(other: Point): Point = {
    if (this == other) {
      this
    }
    // same row
    else if (y == other.y) {
      if (x < other.x - 1) {
        Point(x + 1, y)
      } else if (x > other.x + 1) {
        Point(x - 1, y)
      } else {
        this
      }
    }
    // same column
    else if (x == other.x) {
      if (y < other.y - 1) {
        Point(x, y + 1)
      } else if (y > other.y + 1) {
        Point(x, y - 1)
      } else {
        this
      }
    }
    // Different row/column
    else {
      // farther away on both axis
      if (math.abs(x - other.x) > 1 && math.abs(y - other.y) > 1) {
        var newX = x
        var newY = y
        if (x < other.x) newX += 1
        if (x > other.x) newX -= 1
        if (y < other.y) newY += 1
        if (y > other.y) newY -= 1
        Point(newX, newY)
      }
      // farther away on x axis
      else if (math.abs(x - other.x) > 1) {
        if (x < other.x - 1) {
          Point(x + 1, other.y)
        } else if (x > other.x + 1) {
          Point(x - 1, other.y)
        } else {
          this
        }
      }
      // farther away on y axis
      else {
        if (y < other.y - 1) {
          Point(other.x, y + 1)
        } else if (y > other.y + 1) {
          Point(other.x, y - 1)
        } else {
          this
        }
     }
    }
  }

  override def toString: String = x + "," + y
}

case class Grid(numTails: Int = 1) {
  var grid = 10
  var head: Point = Point(0, 0)
  val tails: ListBuffer[Point] = ListBuffer.fill(numTails)(new Point(0, 0))
  val tailVisits = new mutable.HashSet[Point]
  val allVisits = new mutable.HashSet[Point]
  tailVisits.add(tails.last)
  allVisits.add(tails.last)

  def totalTailVisits: Int = tailVisits.size

  def printGrid(): Unit = {
    val all = allVisits.toList
    val minx = math.min(all.map(_.x).min, -grid)
    val maxx = math.max(all.map(_.x).max, grid)
    val miny = math.min(all.map(_.y).min, -grid)
    val maxy = math.max(all.map(_.y).max, grid)

    println("------------- GRID -------------")

    (maxy to miny by -1) foreach { y => {
      (minx to maxx) foreach { x =>
        val here = Point(x, y)
        if (head == here) {
          print("H")
        } else {
          var printed = false
          for (r <- tails.indices) {
            if (tails(r) == here && !printed) {
              print(r+1)
              printed = true
            }
          }
          if (!printed) {
            if (Point(0, 0) == here) {
              print("S")
            } else {
              tailVisits.foreach(t => {
                if (t == here) {
                  print("#")
                  printed = true
                }
              })
              if (!printed) print(".")
            }
          }
        }
      }
      }
      println()
    }
  }

  def move(moves: List[HeadMove]): Unit = {
    moves.foreach(move)
  }

  def move(move: HeadMove): Unit = {
    (0 until move.count) foreach { x =>
      move.direction match {
        case "U" => head = head.up(1)
        case "D" => head = head.down(1)
        case "L" => head = head.left(1)
        case "R" => head = head.right(1)
      }
      allVisits.add(head)

      moveTail()
    }
  }

  def moveTail(): Unit = {
    // move
    var follow = head
    for (r <- tails.indices) {
      val newTail = tails(r).moveTowards(follow)
      tails(r) = newTail
      follow = tails(r)
      allVisits.add(newTail)
    }

    // record visit
    tailVisits.add(tails.last)
  }
}

object Dec9 extends App {

  private val sample =
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin

  private val sample2 =
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin

  private val sample2_smaller =
    """R 5
      |U 8
      |""".stripMargin

    var lines = Input.readFile("day9.txt").split("\n")
//    lines = sample.split("\n")
//    lines = sample2.split("\n")
//    lines = sample2_smaller.split("\n")

    val moves = lines.map(line => {
      val parts = line.split(" ")
      HeadMove(parts(0), parts(1).toInt)
    })

    println("Data: \n" + moves.mkString("\n"))

    // Part 1: correct answer is 6236
    val grid1 = Grid(1)
    grid1.move(moves.toList)
    println("Part 1 Total tail visits: " + grid1.totalTailVisits)

    // Part 2: correct answer is 2449
    val grid2 = Grid(9)
    grid2.move(moves.toList)
    grid2.printGrid()
    println("Part 2 Total tail visits: " + grid2.totalTailVisits)
}
