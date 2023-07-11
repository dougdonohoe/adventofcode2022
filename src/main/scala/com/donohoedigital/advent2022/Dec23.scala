package com.donohoedigital.advent2022

import scala.collection.mutable
import Const23._
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/23
object Dec23 extends App {

  val sample =
    """.....
      |..##.
      |..#..
      |.....
      |..##.
      |.....""".stripMargin

  val sample2 =
    """..............
      |..............
      |.......#......
      |.....###.#....
      |...#...#.#....
      |....#...##....
      |...#.###......
      |...##.#.##....
      |....#..#......
      |..............
      |..............
      |..............""".stripMargin

  var input = Input.readFile("day23.txt", trim = false).split("\n")

  // uncomment to use sample data
  //input = sample2.split("\n")
  //input = sample.split("\n")

  val points = input.zipWithIndex.flatMap { case (row, r) =>
    row.toCharArray.zipWithIndex.map { case (value, c) =>
      Point23(c, r, value)
    }
  }

  // Part 1 - correct answer is 4138
  val map = Map23(mutable.HashSet.from(points))
  map.printGrid()
  map.move()
  println("Part 1 answer: " + map.part1())

  // Part 2 - correct answer is 1010
  val map2 = Map23(mutable.HashSet.from(points))
  println("Part 2 answer: " + map2.move2())
}

object Const23 {
  val Elf = '#'
  val Open = '.'
}

case class Map23(points: mutable.HashSet[Point23]) {
  val elves = points.filter(_.contents == Elf)

  def hasElf(elf: Point23): Boolean = {
    elves.contains(elf)
  }

  val directions = mutable.Queue.from(List(
    Direction("N"),
    Direction("S"),
    Direction("W"),
    Direction("E"),
  ))

  def move(): Unit = {
    0 until 10 foreach { x =>
      moveElves()
      printGrid()
      directions.addOne(directions.removeHead())
    }
  }

  def move2(): Int = {
    var round: Int = 1
    while (moveElves() > 0) {
      round += 1
      directions.addOne(directions.removeHead())
    }
    round
  }

  def moveElves(): Int = {
    //println("Processing preference: " + directions.head)
    val proposals = elves.map(proposeMove).toList.filter(_.nonEmpty).map(_.get)
    val groups = proposals.groupBy(_.nu)
    val singles = groups.filter(_._2.length == 1)
    singles.values.map(_.head).foreach { p =>
      elves.remove(p.old)
      elves.add(p.nu)
    }
    singles.values.size
  }

  def proposeMove(elf: Point23): Option[Proposal] = {
    val aroundElf = directions.map(d => hasElf(elf, d)).toArray
    val staysPut = !aroundElf.contains(true)
    if (staysPut) return None

    val indexFirstOpen = aroundElf.indexWhere(_ == false)
    if (indexFirstOpen == -1) {
      None
    } else {
      val directionsArray = directions.toArray
      Some(Proposal(elf, elf.moved(directionsArray(indexFirstOpen).spotsToCheck.move)))
    }
  }

  def hasElf(elf: Point23, direction: Direction): Boolean = {
    val aroundElf = direction.spotsToCheck.toList.map(d => hasElf(elf.moved(d)))
    aroundElf.contains(true)
  }

  def printGrid(): Unit = {
    val elvesList = elves.toList
    val minX = elvesList.map(_.x).min
    val maxX = elvesList.map(_.x).max
    val minY = elvesList.map(_.y).min
    val maxY = elvesList.map(_.y).max
    minY to maxY foreach { y =>
      minX to maxX foreach { x =>
        if (hasElf(Point23(x, y, Elf))) {
          print("#")
        } else {
          print(".")
        }
      }
      println
    }
  }

  def part1(): Int = {
    val elvesList = elves.toList
    val minX = elvesList.map(_.x).min
    val maxX = elvesList.map(_.x).max
    val minY = elvesList.map(_.y).min
    val maxY = elvesList.map(_.y).max
    ((maxX - minX + 1) * (maxY - minY + 1)) - elvesList.size
  }
}

case class Proposal(old: Point23, nu: Point23)

case class Point23(x: Int, y: Int, contents: Char) {
  def moved(delta: Delta): Point23 = {
    this.copy(x = x + delta.x, y = y + delta.y)
  }
}

// During the first half of each round, each Elf considers the eight positions adjacent to themself.
// If no other Elves are in one of those eight positions, the Elf does not do anything during
// this round. Otherwise, the Elf looks in each of four directions in the following
// order and proposes moving one step in the first valid direction:
//
//If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
//If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
//If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
//If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
case class Delta(x: Int, y: Int)

// move is checked for empty and where the move would go to
// diag1 / diag2 most be empty
// each represents delta from current point
case class Process(move: Delta, diag1: Delta, diag2: Delta) {
  val toList = List(move, diag1, diag2)
}

case class Direction(dir: String) {
  val spotsToCheck: Process = {
    dir match {
      // (x,y)
      // x: change east/west
      // y: change north south
      case "W" => Process(Delta(-1, 0), Delta(-1, 1), Delta(-1, -1)) // W, NW, or SW
      case "E" => Process(Delta(1, 0), Delta(1, -1), Delta(1, 1)) // E, NE, or SE
      case "N" => Process(Delta(0, -1), Delta(-1, -1), Delta(1, -1)) // N, NE, or NW
      case "S" => Process(Delta(0, 1), Delta(-1, 1), Delta(1, 1)) // S, SE, or SW
    }
  }
}