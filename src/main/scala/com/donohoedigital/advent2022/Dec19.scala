package com.donohoedigital.advent2022

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// https://adventofcode.com/2022/day/19
object Dec19 extends App {

  val sample = """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                 |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin
  var input = Input.readFile("day19.txt", trim = false).split("\n")

  // Uncomment to run on sample data
  input = sample.split("\n")

  // parsing: (all on one line)
  //          Blueprint 1: Each ore robot costs 4 ore. \
  //          Each clay robot costs 4 ore. \
  //          Each obsidian robot costs 4 ore and 18 clay. \
  //          Each geode robot costs 4 ore and 9 obsidian.
  val recipes = input.map { line => {
    val justNumbers = line.replaceAll("[A-Za-z :.]+", " ").trim
    val n = justNumbers.split(" ").map(_.toInt)
    Recipe(n(0), Cost(ore = n(1)), Cost(ore = n(2)), Cost(ore = n(3), clay = n(4)), Cost(ore = n(5), obsidian = n(6)))
  }
  }

  //recipes.foreach(println)

  println("S1: " + recipes(0).simulate())
  println("S2: " + recipes(1).simulate())

  // NOTE: Never got to a solution for Part 1 or Part 2
}

case class State(var minute: Int,
                 var oreR: Int = 0, var clayR: Int = 0, var obsidianR: Int = 0, var geodeR: Int = 0,
                 var ore: Int = 0, var clay: Int = 0, var obsidian: Int = 0, var geode: Int = 0,
                 var indent:String = "")
{
  def tick(): Unit = {
    ore += oreR
    clay += clayR
    obsidian += obsidianR
    geode += geodeR
  }

  override def toString: String = {
    indent + "Min=" + minute +
      " ore="+ore+ " robot="+oreR+
      " | clay="+clay+" robot="+clayR+
      " | obs="+obsidian+" robot="+obsidianR+
      " | GEODE="+geode+" robot="+geodeR
  }
}

case class Cost(ore: Int = 0, clay: Int = 0, obsidian: Int = 0) {
  def canAfford(state: State): Boolean = {
    ore <= state.ore && clay <= state.clay && obsidian <= state.obsidian
  }
  def purchase(state: State): Unit = {
    state.ore -= ore
    state.clay -= clay
    state.obsidian -= obsidian
  }
}

case class Recipe(num: Int, oreCost: Cost, clayCost: Cost, obsidianCost: Cost, geodeCost: Cost,
                  verbose: Boolean = false) {

  var count = 0
  var maxRobots = 4

  def simulate(state: State = State(0, 1)): State = {
    state.minute += 1
    count += 1

    if (count % 10000000 == 0) {
      println("COUNT " + count)
    }

    if (state.minute > 24) {
      return state
    }

    if (verbose) println(state)

    val scenarios = new ListBuffer[State]
    if (state.oreR < maxRobots && oreCost.canAfford(state)) {
      val nu = state.copy(indent = state.indent + " ")
      nu.tick()
      oreCost.purchase(nu)
      nu.oreR += 1
      if (verbose) println(state.indent + "PURCHASED ORE " + nu)
      scenarios += simulate(nu)
    }
    if (state.clayR < maxRobots && clayCost.canAfford(state)) {
      val nu = state.copy(indent = state.indent + " ")
      nu.tick()
      clayCost.purchase(nu)
      nu.clayR += 1
      if (verbose) println(state.indent + "PURCHASED CLAY " + nu)
      scenarios += simulate(nu)
    }
    if (state.obsidianR < maxRobots && obsidianCost.canAfford(state)) {
      val nu = state.copy(indent = state.indent + " ")
      nu.tick()
      obsidianCost.purchase(nu)
      nu.obsidianR += 1
      if (verbose) println(state.indent + "PURCHASED OBSIDIAN " + nu)
      scenarios += simulate(nu)
    }
    if (state.geodeR < maxRobots && geodeCost.canAfford(state)) {
      val nu = state.copy(indent = state.indent + " ")
      nu.tick()
      geodeCost.purchase(nu)
      nu.geodeR += 1
      if (verbose) println(state.indent + "PURCHASED GEODE " + nu)
      scenarios += simulate(nu)
    }
    val nu = state.copy(indent = state.indent + " ")
    nu.tick()
    if (verbose) println(state.indent + "NOTHING ----- " + nu)
    scenarios += simulate(nu)

    // return state with highest geodes
    scenarios.sortBy(_.geode).reverse.head
  }
}