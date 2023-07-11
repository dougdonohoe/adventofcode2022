package com.donohoedigital.advent2022

case class Hand(themInput: String, meInput: String) {
  // 1 for Rock, 2 for Paper, and 3 for Scissors
  val Rock = 1
  val Paper = 2
  val Scissors = 3

  val Win = 6
  val Lose = 0
  val Draw = 3

  val hand: Map[String, Int] = Map(
    "A" -> Rock, "B" -> Paper, "C" -> Scissors,
    "X" -> Rock, "Y" -> Paper, "Z" -> Scissors,
  )

  val them: Int = {
    hand(themInput)
  }

  val me: Int = {
    hand(meInput)
  }

  def pick(): Int = {
    if (me == 1) {
      pickLoser()
    } else if (me == 2) {
      them
    } else { // meInt == 3 {
      pickWinner()
    }
  }

  def pickWinner(): Int = {
    if (them == Scissors) {
      Rock
    } else if (them == Paper) {
      Scissors
    } else {
      Paper
    }
  }

  def pickLoser(): Int = {
    if (them == Scissors) {
      Paper
    } else if (them == Paper) {
      Rock
    } else {
      Scissors
    }
  }

  def rockPaperScissors(themParam: Int, meParam: Int): Int = {
    if (themParam == meParam) {
      Draw
    }
    else if (themParam == Rock && meParam == Scissors) {
      Lose
    }
    else if (themParam == Rock && meParam == Paper) {
      Win
    }
    else if (themParam == Scissors && meParam == Rock) {
      Win
    }
    else if (themParam == Scissors && meParam == Paper) {
      Lose
    }
    else if (themParam == Paper && meParam == Rock) {
      Lose
    }
    else // if ( themInt == Paper && meInt == Scissors {
      Win
  }
}

object Dec2 {
  def main(args: Array[String]): Unit = {
    val lines = Input.readFile("day2.txt").split("\n")

    val rounds = lines.map(line => {
      val parts = line.split(" ")
      Hand(parts(0), parts(1))
    })

    val part1 = rounds.map(part =>
      part.me + part.rockPaperScissors(part.them, part.me)
    )
    println("Total Part1: " + part1.sum)

    val part2 = rounds.map(part => {
      val pick = part.pick()
      pick + part.rockPaperScissors(part.them, pick)
    })
    println("Total Part2: " + part2.sum)
  }
}
