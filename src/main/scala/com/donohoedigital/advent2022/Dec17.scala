package com.donohoedigital.advent2022

import scala.collection.mutable

// https://adventofcode.com/2022/day/17
object Dec17 extends App {

  val input = Input.readFile("day17.txt")

  // uncomment test input
  val sample = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

  // Part 1 sample - correct answer is 3068
  val simS = Simulator(2022, sample)
  simS.run()
  println("Tallest Part 1 sample (correct is 3068): " + simS.totalHeight + " (lowestY: " + simS.lowestY + ")")
  println()

  // Part 1 - correct answer is 3193
  val sim = Simulator(2022, input)
  sim.run()
  println("Tallest Part 1 (correct is 3193): " + sim.totalHeight + " (lowestY: " + sim.lowestY + ")")
  println()

  // Part 2 sample -  correct answer is 1514285714288)
  val sim2S = Simulator(1000000000000L, sample)
  sim2S.run()
  println("Tallest Part 2 sample (correct is 1514285714288): " + sim2S.totalHeight)
  println()

  // Part 2 - correct answer is 1577650429835
  val sim2 = Simulator(1000000000000L, input)
  sim2.run()
  println("Tallest Part 2 (correct is 1577650429835): " + sim2.totalHeight)
}

object D17Const {
  val Free = 0
  val Wall = 3
  val Floor = 4
  val RockSettled = 1
  val RockFree = 2
}

case class Simulator(totalRocks: Long, flow: String, verbose: Boolean = false) {

  val maxWidth = 9 // 7 wide plus two walls
  var highestY = 0L
  var extraY = 0L
  var lowestY = 0L

  def totalHeight: Long = highestY + extraY

  def printGrid(): Unit = {
    highestY + 9 to lowestY by -1 foreach { y =>
      0 until maxWidth foreach { x =>
        grid(x, y) match {
          case D17Const.RockSettled => print("#")
          case D17Const.RockFree => print("@")
          case D17Const.Wall => print("|")
          case D17Const.Floor => print("_")
          case D17Const.Free => print(".")
        }
      }
      if (y == lowestY) print(" lowest=" + lowestY) else print(" y=" + y)
      println()
    }
  }

  val gridMap: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()

  def grid(x: Int, y: Long): Int = {
    if (x == 0 || x == 8) D17Const.Wall
    else if (y <= lowestY) D17Const.Floor
    else gridMap.getOrElse(x + "-" + y, D17Const.Free)
  }

  def set(x: Int, y: Long, value: Int): Unit = {
    gridMap(x + "-" + y) = value
  }

  val cycleDetector = new mutable.HashMap[String, CycleInfo]()

  case class CycleInfo(highestY: Long, rockNum: Long) {
    override def toString: String = {
      "highestY=" + highestY + " rockNum=" + rockNum
    }
  }

  def isCycle(rock: Rock): Option[CycleInfo] = {
    val key = rock.name + "-" + gasIndex.toString + "-" + rockSignature()

    cycleDetector.get(key) match {
      case None =>
        cycleDetector(key) = CycleInfo(highestY, rockNum)
        None

      case Some(info) =>
        cycleDetector(key) = CycleInfo(highestY, rockNum)
        // only a cycle if an even multiple of all rocks have passed
        if ((rockNum - info.rockNum) % 5 == 0) {
          Some(info)
        } else {
          None
        }
    }
  }

  var rockNum = 0L

  def run(): Unit = {
    while (rockNum < totalRocks) {
      process(chooseRock(rockNum))
      if ((rockNum + 1) % 1000 == 0) {
        clean()
      }
      rockNum += 1
    }
  }

  def rockSignature(): String = {
    highestYs.map(y => highestY - y).mkString("--")
  }

  def clean(): Unit = {
    val ys = highestYs
    val newLowestY = ys.min
    if (newLowestY != lowestY) {
      var y = newLowestY
      while (y >= lowestY) {
        1 to 7 map { x =>
          gridMap.remove(x + "-" + y)
        }
        y -= 1
      }
      lowestY = newLowestY
    }
  }

  private def highestYs: Seq[Long] = {
    val ys = 1 to 7 map { x =>
      var y = highestY
      var blocked = lowestY
      var done = false
      while (y > lowestY && !done) {
        if (grid(x, y) != D17Const.Free) {
          blocked = y
          done = true
        }
        y -= 1
      }
      blocked
    }
    ys
  }

  private var stopCycleDetection = false
  private var gas = true

  def process(rock: Rock): Unit = {

    if (!stopCycleDetection)
      isCycle(rock) match {
        case Some(info) =>
          val cycleNumRocks = rockNum - info.rockNum
          val cycleHeight = highestY - info.highestY
          val rocksLeft = totalRocks - rockNum
          val loops = rocksLeft / cycleNumRocks
          val heightToAdd = loops * cycleHeight
          val rocksToUse = loops * cycleNumRocks
          extraY = heightToAdd
          rockNum += rocksToUse
          stopCycleDetection = true
        case None =>
      }

    rock.location = P2(3, highestY + 4)

    if (!addRock(rock)) {
      println("UNABLE TO ADD ROCK " + rock + " at " + rock.location)
      System.exit(-1)
    }
    if (verbose) {
      println("Processing: " + rock)
      printGrid()
    }
    // alternate: jet + fall down one
    while (!rock.settled) {
      if (gas) {
        val flow = nextFlow
        if (verbose) println("GAS " + (if (flow == 1) ">" else "<"))
        removeRock(rock)
        rock.moveLocation(flow, 0)
        if (!canMove(rock)) {
          rock.moveLocation(-flow, 0)
        }
        addRock(rock)
        if (verbose) printGrid()
      } else {
        if (verbose) println("DOWN 1")
        removeRock(rock)
        rock.moveLocation(0, -1)
        if (!canMove(rock)) {
          if (verbose) println("FROZEN " + rock)
          rock.moveLocation(0, 1)
          if (!addRock(rock, settled = true)) {
            println("UNABLE TO ADD SETTLED ROCK " + rock + " at " + rock.location)
            System.exit(-1)
          }
          rock.settled = true
          highestY = math.max(highestY, rock.gridLocations.map(_.y).max)
        } else {
          if (!addRock(rock)) {
            println("UNABLE TO ADD MOVED ROCK " + rock + " at " + rock.location)
            System.exit(-1)
          }
        }
        if (verbose) printGrid()
      }
      gas = !gas
    }
    gas = true
  }

  def canMove(rock: Rock): Boolean = {
    val locations = rock.gridLocations
    locations.forall(isFree)
  }

  def addRock(rock: Rock, settled: Boolean = false): Boolean = {
    val locations = rock.gridLocations
    if (!locations.forall(isFree)) {
      false
    } else {
      locations.foreach { p =>
        set(p.x, p.y, if (settled) D17Const.RockSettled else D17Const.RockFree)
      }
      true
    }
  }

  def removeRock(rock: Rock): Unit = {
    val locations = rock.gridLocations
    locations.foreach { p =>
      set(p.x, p.y, D17Const.Free)
    }
  }

  def isFree(p: P2): Boolean = {
    grid(p.x, p.y) == D17Const.Free
  }

  private var gasIndex = 0

  def nextFlow: Int = {
    val direction = flow.charAt(gasIndex % flow.length) match {
      case '>' => 1
      case '<' => -1
    }
    gasIndex = gasIndex + 1
    if (gasIndex == flow.length) gasIndex = 0
    direction
  }

  def chooseRock(num: Long): Rock = {
    ((num + 1) % 5) match {
      case 1 => FallingPieces.hLine.copy()
      case 2 => FallingPieces.plus.copy()
      case 3 => FallingPieces.ell.copy()
      case 4 => FallingPieces.vLine.copy()
      case 0 => FallingPieces.square.copy()
    }
  }
}

case class P2(var x: Int, var y: Long) {
  def add(loc: P2): P2 = {
    P2(x + loc.x, y + loc.y)
  }
}

case class Rock(name: String, points: List[P2]) {
  var settled = false
  var location: P2 = _

  def moveLocation(x: Int, y: Int): Unit = {
    location = P2(location.x + x, location.y + y)
  }

  def gridLocations: List[P2] = points.map(_.add(location))
}

object FallingPieces {

  // Y increase as one goes vertical (0 is lower) x is left to right

  //   ####
  val hLine: Rock = Rock("hline", List(P2(0, 0), P2(1, 0), P2(2, 0), P2(3, 0)))

  //  .#.
  //  ###
  //  .#.
  val plus: Rock = Rock("plus", List(
    /*    */ P2(1, 2),
    P2(0, 1), P2(1, 1), P2(2, 1),
    /*    */ P2(1, 0)))

  //  ..#
  //  ..#
  //  ###
  val ell: Rock = Rock("ell", List(
    /*             */ P2(2, 2),
    /*             */ P2(2, 1),
    P2(0, 0), P2(1, 0), P2(2, 0),
  ))

  //  #
  //  #
  //  #
  //  #
  val vLine: Rock = Rock("vline", List(
    P2(0, 3),
    P2(0, 2),
    P2(0, 1),
    P2(0, 0),
  ))

  // ##
  // ##
  val square: Rock = Rock("square", List(
    P2(0, 0), P2(1, 0),
    P2(0, 1), P2(1, 1),
  ))
}