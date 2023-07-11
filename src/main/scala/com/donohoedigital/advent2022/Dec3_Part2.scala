package com.donohoedigital.advent2022

object Dec3_Part2 {
  def main(args: Array[String]): Unit = {
    val lines = Input.readFile("day3.txt").split("\n")
    // Parse the input
    val groups = lines.grouped(3)

    // Define a function that takes a group of three lines and returns the common item type
    def commonItemType(lines: Array[String]): String = {
      // Create a set of all the unique item types in the first line
      val itemTypes = lines.head.toSet

      // Go through the second and third lines and find the item types that are common to all three lines
      val commonTypes = lines.drop(1).foldLeft(itemTypes) { (types, line) =>
        types.intersect(line.toSet)
      }

      // Return the first common item type (there should only be one)
      commonTypes.head.toString
    }

    // Find the common item type for each group and sum up their priorities
    val resultX = groups.map(commonItemType).map(priority)
    val result = resultX.sum

    // Print the result
    println(result) // should print 70
  }

  def priority(commonItemType: String): Int = {
    val asciiValue = commonItemType(0).toInt
    if (commonItemType(0).isLower) asciiValue - 96
    else asciiValue - 38
  }
}
