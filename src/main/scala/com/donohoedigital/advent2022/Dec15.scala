package com.donohoedigital.advent2022

import annotation.{tailrec => tco}

// https://adventofcode.com/2022/day/15
object Dec15 extends App {

  val sample =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
      |Sensor at x=9, y=16: closest beacon is at x=10, y=16
      |Sensor at x=13, y=2: closest beacon is at x=15, y=3
      |Sensor at x=12, y=14: closest beacon is at x=10, y=16
      |Sensor at x=10, y=20: closest beacon is at x=10, y=16
      |Sensor at x=14, y=17: closest beacon is at x=10, y=16
      |Sensor at x=8, y=7: closest beacon is at x=2, y=10
      |Sensor at x=2, y=0: closest beacon is at x=2, y=10
      |Sensor at x=0, y=11: closest beacon is at x=2, y=10
      |Sensor at x=20, y=14: closest beacon is at x=25, y=17
      |Sensor at x=17, y=20: closest beacon is at x=21, y=22
      |Sensor at x=16, y=7: closest beacon is at x=15, y=3
      |Sensor at x=14, y=3: closest beacon is at x=15, y=3
      |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin

  var input = Input.readFile("day15.txt", trim = false).split("\n")
  var target = 2000000

  // Uncomment to run on sample data
  //  input = sample.split("\n")
  //  target = 10

  // Parse lines like:
  //   'Sensor at x=2793338, y=1910659: closest beacon is at x=2504930, y=2301197'
  val data = input.map { line =>
    val nums = line.replace("Sensor at x=", "").
      replace(" y=", "").
      replace(": closest beacon is at x=", ",").
      split(",")
    Data14(nums(0).toInt, nums(1).toInt, nums(2).toInt, nums(3).toInt)
  }

  // Helper to merge coverage ranges into minimum number of ranges
  def mergedCoverage(row: Int, includeBeacons: Boolean = false): List[Range] = {
    val coverage2 = data.flatMap(_.coverage(row, includeBeacons)).toList
    Util.merge(coverage2)
  }

  println("Data: \n" + data.mkString("\n") + "\n")

  // Part 1 - correct answer is 5166077 (for sample, it is 26)
  val c2 = mergedCoverage(target)
  println("Coverage merged: " + c2)
  println("Part 1 coverage at row " + target + " is " + c2.map(_.length).sum)
  println()

  // Part 2 - correct answer is 13071206703981 (for sample, it is 56000011)
  0 until (target * 2) foreach { y =>
    val coverage = mergedCoverage(y, includeBeacons = true)
    if (coverage.length > 1) {
      val x = (coverage.head.end + 1).toLong
      println("y=" + y + " x=" + x + " coverage " + coverage)
      println("Part 2: Tuning Frequency: " + (y.toLong + (4000000 * x)))
    }
  }
}

case class Data14(sensorX: Int, sensorY: Int, beaconX: Int, beaconY: Int) {
  val manhattanDistance: Int = {
    math.abs(sensorX - beaconX) + math.abs(sensorY - beaconY)
  }

  def coverage(y: Int, includeBeacons: Boolean = false): List[Range] = {
    val deltaFromSensor = math.abs(y - sensorY)
    val distAtY = manhattanDistance - deltaFromSensor
    val range = Range.inclusive(sensorX - distAtY, sensorX + distAtY)
    if (!includeBeacons && range.contains(beaconX) && y == beaconY) {
      val tuple = range.splitAt(range.indexOf(beaconX))
      List(tuple._1, tuple._2.tail).filter(_.nonEmpty)
    } else {
      List(range).filter(_.nonEmpty)
    }
  }

  override def toString: String = {
    "s(" + sensorX + "," + sensorY + ") b(" + beaconX + "," + beaconY + ") dist=" + manhattanDistance
  }
}

object Util {
  // Code to collapse a List of ranges into a minimum number of ranges
  // Adapted from (changed to support inclusive ranges)
  // https://stackoverflow.com/questions/9218891/how-to-functionally-merge-overlapping-number-ranges-from-a-list

  @tco final def collapse(rs: List[Range], sep: List[Range] = Nil): List[Range] = rs match {
    case x :: y :: rest =>
      if (y.start - 1 > x.end) collapse(y :: rest, x :: sep)
      else collapse(Range.inclusive(x.start, x.end max y.end) :: rest, sep)
    case _ =>
      (rs ::: sep).reverse
  }

  def merge(rs: List[Range]): List[Range] = collapse(rs.sortBy(_.start))
}
