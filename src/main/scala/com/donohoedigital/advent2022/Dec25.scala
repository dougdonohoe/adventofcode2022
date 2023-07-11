package com.donohoedigital.advent2022

// https://adventofcode.com/2022/day/25
object Dec25 extends App {

  val sample =
    """=
      |-
      |0
      |1
      |2
      |1=-0-2
      |12111
      |2=0=
      |21
      |2=01
      |111
      |20012
      |112
      |1=-1=
      |1-12
      |12
      |1=
      |122""".stripMargin

  var input = Input.readFile("day25.txt", trim = false).split("\n")

  // uncomment to use sample data
  ///input = sample.split("\n")

  val snafus = input.map(Snafu)
  snafus.foreach { s =>
    println(s.digits + " = " + s.toLong)
  }

  // Part 1 correct answer is 122-0==-=211==-2-200
  println("SUM: " + snafus.map(_.toLong).sum)
  println("SUM in Snafu: " + Helper.toSnafu(snafus.map(_.toLong).sum))
}

case class Snafu(digits: String) {

  def toLong: Long = {
    val values = digits.reverse.zipWithIndex.map { case (digit, idx) =>
      val base = math.pow(5, idx).toLong
      val num = digit match {
        case '=' => -2 * base
        case '-' => -1 * base
        case '0' => 0 * base
        case '1' => 1 * base
        case '2' => 2 * base
      }
      num
    }
    values.sum
  }
}

object Helper {

  def toSnafu(i: Long): Snafu = {
    val sb = new StringBuilder()
    var total = i
    val digits = "012=-"
    while (total > 0) {
      val digit = digits.charAt((total % 5).toInt)
      sb.insert(0, digit)
      total = (2 + total) / 5
    }
    Snafu(sb.toString())
  }
}