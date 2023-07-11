package com.donohoedigital.advent2022

object Dec6 {
  def main(args: Array[String]): Unit = {

    val line = Input.readFile("day6.txt")

    def calcAnswer(length: Int): Unit = {
      for (i <- length - 1 until line.length) {
        val segment = line.substring(i - (length - 1), i + 1)
        if (segment.toSet.size == length) {
          println("Length: " + length + ", Segment: " + segment + ", i: " + i + ", answer: " + (i + 1))
          return
        }
      }
    }

    calcAnswer(4) // Part 1
    calcAnswer(14) // Part2

  }
}
