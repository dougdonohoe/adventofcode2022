package com.donohoedigital.advent2022

object Dec3 {
  def main(args: Array[String]): Unit = {
    val lines = Input.readFile("day3.txt").split("\n")
    // Initialize sum of priorities
    var sumPriorities = 0

    // Iterate over lines of input
    lines.foreach { line =>
      // Split line in half
      val half = line.length / 2
      val compartment1 = line.substring(0, half)
      val compartment2 = line.substring(half)

      // Find common item type
      var commonItemType = '\u0000'
      compartment1.foreach { c =>
        if (compartment2.contains(c)) {
          commonItemType = c
        }
      }

      // Calculate priority of common item type
      val asciiValue = commonItemType.toInt
      val priority =
        if (commonItemType.isLower) asciiValue - 96
        else asciiValue - 38

      // Add priority to total sum
      sumPriorities += priority
    }

    // Print sum of priorities
    println(sumPriorities)
  }
}
