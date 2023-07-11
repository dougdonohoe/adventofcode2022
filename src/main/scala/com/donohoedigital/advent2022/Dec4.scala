package com.donohoedigital.advent2022

case class Pair(elf1: Range, elf2: Range) {
  def overlapsAll(): Boolean = {
    elf1.intersect(elf2) == elf1 || elf2.intersect(elf1) == elf2
  }

  def overlapPartial(): Boolean = {
    elf1.intersect(elf2).nonEmpty
  }
}

object Dec4 {
  def main(args: Array[String]): Unit = {
    val lines = Input.readFile("day4.txt").split("\n")
    val pairs = lines.map(x => {
      val elves = x.split(",")
      val ranges = elves.map(e => {
        val nums = e.split("-")
        Range.inclusive(nums(0).toInt, nums(1).toInt, 1)
      })
      Pair(ranges(0), ranges(1))
    })

    val overlappingAll = pairs.filter(_.overlapsAll())
    println("Part 1: Total overlapping: " + overlappingAll.length)

    val partialOverlapping = pairs.filter(_.overlapPartial())
    println("Part 1: Partial overlapping: " + partialOverlapping.length)
  }
}
