package com.donohoedigital.advent2022

import scala.collection.mutable
import Const24._

import scala.annotation.tailrec

// https://adventofcode.com/2022/day/24
object Dec24 extends App {

  val sample =
    """#.#####
      |#.....#
      |#>....#
      |#.....#
      |#...v.#
      |#.....#
      |#####.#""".stripMargin

  val sample2 =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin

  var input = Input.readFile("day24.txt", trim = false).split("\n")

  // uncomment to use sample data
  //input = sample2.split("\n")
  //input = sample.split("\n")

  val width = input.head.length
  val height = input.length

  val points = input.zipWithIndex.map { case (row, y) =>
    row.toCharArray.zipWithIndex.map { case (value, x) =>
      val wall = y == 0 || y == (height - 1) || x == 0 || x == (width - 1)
      val door = wall && value == Open
      val blizzard = value match {
        case Up => UpBlizzard()
        case Down => DownBlizzard()
        case Left => LeftBlizzard()
        case Right => RightBlizzard()
        case _ => null.asInstanceOf[Blizzard]
      }
      val set = new mutable.HashSet[Blizzard]()
      if (blizzard != null) {
        set.add(blizzard)
      }
      Point24(x, y, set, wall, door)
    }
  }

  val cavern = Cavern(points)
  cavern.printGrid()
  val leastMult = lcm(List(cavern.width - 2, cavern.height - 2))
  private val range = 0 until leastMult.toInt
  val allCaverns = range.map { x =>
    if (x == 0) cavern.copyDeep() else cavern.moveBlizzards()
  }.toArray

  val start = cavern.points(0).find(_.isDoor).get
  val end = cavern.points(cavern.height - 1).find(_.isDoor).get

  // part 1 correct answer is 247
  val finder = PathFinder(start, end, 0, allCaverns)
  val part1 = finder.search()
  println("Part 1 answer is " + part1)
  println()

  // part 2 correct answer is 728
  val back = PathFinder(end, start, part1, allCaverns).search()
  val backAgain = PathFinder(start, end, back, allCaverns).search()
  println("Part 2 answer is " + backAgain)
}

case class PathFinder(start: Point24, end: Point24, startMinute: Int, allCaverns: Array[Cavern]) {
  val queue = new mutable.Queue[SearchFrom]()
  val visited = new mutable.HashSet[SearchFrom]()

  def minute(min: Int): Cavern = {
    allCaverns(min % allCaverns.length)
  }

  // return minute we reach the end
  def search(): Int = {
    queue += SearchFrom(startMinute, start.x, start.y)
    searchQueue()
  }

  def searchQueue(): Int = {
    var minFound = Int.MaxValue
    while (queue.nonEmpty) {
      val next = queue.removeHead()
      val nextMinute = next.minute + 1

      if (!visited.contains(next)) {
        visited += next

        if (nextMinute < minFound) {

          val blizzard = minute(nextMinute)
          //println("Searching from " + next)
          //blizzard.printGrid(next)

          if (next.x == end.x && next.y == end.y) {
            println("FOUND path at minute " + next.minute)
            if (next.minute < minFound) {
              println("NEW LOW of " + next.minute)
              minFound = next.minute
            }
          }
          val moves = List(
            (next.x, next.y), // wait
            (next.x - 1, next.y), // left
            (next.x + 1, next.y), // right
            (next.x, next.y - 1), // up
            (next.x, next.y + 1), //down
          )
          moves.foreach(t => {
            if (blizzard.isEmpty(t._1, t._2)) {
              queue += SearchFrom(nextMinute, t._1, t._2)
            }
          })
        }
      }
    }
    minFound
  }
}

case class SearchFrom(minute: Int, x: Int, y: Int)

case class Cavern(points: Array[Array[Point24]]) {
  val width = points(0).length
  val height = points.length

  def get(x: Int, y: Int): Point24 = points(y)(x)

  def copyDeep(): Cavern = {
    Cavern(points.map(_.map(x => x.copy(contents = mutable.HashSet.from(x.contents)))))
  }

  def isEmpty(x: Int, y: Int): Boolean = {
    if (x < 0 || x >= width || y < 0 || y >= height) {
      false
    } else {
      val p = get(x, y)
      p.isDoor || (!p.isWall && p.contents.isEmpty)
    }
  }

  def moveBlizzards(): Cavern = {
    1 until height - 1 foreach { y =>
      1 until width - 1 foreach { x =>
        val p = get(x, y)
        p.contents.foreach { blizzard =>
          var newY: Int = y
          var newX: Int = x
          blizzard match {
            case DownBlizzard() =>
              if (y == (height - 2)) {
                newY = 1
              } else {
                newY = y + 1
              }

            case UpBlizzard() =>
              if (y == 1) {
                newY = height - 2
              } else {
                newY = y - 1
              }

            case LeftBlizzard() =>
              if (x == 1) {
                newX = width - 2
              } else {
                newX = x - 1
              }

            case RightBlizzard() =>
              if (x == (width - 2)) {
                newX = 1
              } else {
                newX = x + 1
              }
          }
          get(newX, newY).temp.add(blizzard)
        }
      }
    }
    1 until height - 1 foreach { y =>
      1 until width - 1 foreach { x =>
        get(x, y).moveTemp()
      }
    }
    copyDeep()
  }

  def printGrid(elves: SearchFrom = null): Unit = {
    0 until height foreach { y =>
      0 until width foreach { x =>
        val p = get(x, y)
        if (elves != null && elves.x == x && elves.y == y) {
          print("E")
        }
        else if (p.isDoor) {
          print(".")
        } else if (p.isWall) {
          print("#")
        } else if (p.contents.isEmpty) {
          print(".")
        } else if (p.contents.size > 1) {
          print(p.contents.size)
        } else {
          print(p.contents.head.char)
        }
      }
      println()
    }
  }
}

case class Point24(x: Int, y: Int, contents: mutable.HashSet[Blizzard], isWall: Boolean, isDoor: Boolean) {
  var temp = new mutable.HashSet[Blizzard]()

  def moveTemp(): Unit = {
    contents.clear
    contents.addAll(temp)
    temp.clear()
  }
}

sealed trait Blizzard {
  def char: Char
}

case class UpBlizzard() extends Blizzard() {
  val char = Up
}

case class DownBlizzard() extends Blizzard() {
  val char = Down
}

case class LeftBlizzard() extends Blizzard() {
  val char = Left
}

case class RightBlizzard() extends Blizzard() {
  val char = Right
}

object Const24 {
  val Wall = "#"
  val Up = '^'
  val Down = 'v'
  val Left = '<'
  val Right = '>'
  val Open = '.'

  @tailrec
  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

}