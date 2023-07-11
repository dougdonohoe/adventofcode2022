package com.donohoedigital.advent2022

import scala.collection.mutable.ListBuffer

case class Instruction(addx: Int, noop: Boolean = false)

object Dec10 extends App {
  var lines = Input.readFile("day10.txt", trim = false).split("\n")

  val instructions = lines.map { line =>
    if (line == "noop") {
      Instruction(0, noop = true)
    } else {
      Instruction(line.replace("addx ", "").toInt)
    }
  }

  val xDuring = new ListBuffer[Int]
  var x = 1

  instructions.foreach { i =>
    if (i.noop) {
      xDuring += x
    } else {
      xDuring += x
      xDuring += x
      x = x + i.addx
    }
  }

  // Part 1 - correct answer is 12840
  // Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles.
  // What is the sum of these six signal strengths?
  val nums = List(20, 60, 100, 140, 180, 220).map(num =>
    xDuring(num - 1) * num
  )
  println("Part 1 sum: " + nums.sum)

  // Part 2 - correct answer is ZKJFBJFZ
  val pixels = xDuring.zipWithIndex.map { case (x, idx) =>
    if (math.abs(x - idx % 40) <= 1) "#" else "."
  }
  val monitor = pixels.grouped(40).map(_.mkString("")).mkString("\n")
  println(monitor)
}

