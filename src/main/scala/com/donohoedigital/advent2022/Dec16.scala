package com.donohoedigital.advent2022

import org.jgrapht.alg.shortestpath.DijkstraShortestPath
import org.jgrapht.graph.DefaultUndirectedGraph
import org.jgrapht.{Graph, GraphPath}

import java.util.Date
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Dec16Global {
  var minutes = 30
}

// https://adventofcode.com/2022/day/16
object Dec16 extends App {

  val sample = """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                 |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                 |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                 |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                 |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                 |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                 |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                 |Valve HH has flow rate=22; tunnel leads to valve GG
                 |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                 |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

  var input = Input.readFile("day16.txt", trim = false).split("\n")

  // uncomment to use sample data - valid path part 1 is DD BB JJ HH EE CC
  //input = sample.split("\n")

  // Input looks like "Valve AA has flow rate=0; tunnels lead to valves VB, XL, XZ, XW, UX"
  val valves = input.map{ line =>
    val pattern = "Valve (..) has flow rate=(\\d+); [a-z ]+ (.*)".r
    val pattern(name, rate, leadsTo) = line
    Valve(name, rate.toInt, leadsTo.split(", ").toList)
  }

  // Part 1 - correct answer is Solution(XQ VP VM TR DO KI HN,1845)
  val map = ValveMap(valves)
//  println("---walk---")
//  val walk = map.walk()
//  println("Part 1 solution: " + walk.solutions.sortBy(_.totalFlow).reverse.head)

  // Part 2 - correct answer is 2286
  Dec16Global.minutes = 26 // clock shortens
  println("---walk 2---")
  val walk2 = map.walk(2)
  println("Part 2 solution: " + walk2.solutions.sortBy(_.totalFlow).reverse.head)
}

///
/// HELPER CLASSES BELOW
///

case class ValvePlus(valve: Valve, clock: Int) {
  def value: Int = valve.totalFlow(clock)

  override def toString: String = {
    valve.name + " [" + valve.flowRate + "] (" + clock + ") total=" + value
  }
}

case class Solution(path: String, totalFlow: Int)

case class Walk(map: ValveMap) {
  val solutions = new mutable.ListBuffer[Solution]
  val solutionsFound = new mutable.HashSet[String]
  val visited = new mutable.HashMap[Valve, Int]()

  def toList: List[ValvePlus] = {
    visited.map { case (v, clock) =>
      ValvePlus(v, clock)
    }.toList.sortBy(_.clock)
  }

  def path: String = {
    toList.map(_.valve.name).mkString(" ")
  }

  def totalFlow: Int = {
    toList.map(_.value).sum
  }

  def recordSolution(): Boolean = {
    val p = path
    if (!solutionsFound.contains(p)) {
      solutions += Solution(p, totalFlow)
      solutionsFound += p
      if (solutions.length % 1000 == 0) {
        println("Solution count: " + solutions.length + " " + new Date())
      }
      true
    } else {
      false
    }
  }

  def unvisitedCount: Int = {
    map.valves.count(v => v.flowRate > 0 && !visited.contains(v))
  }

  def paths(from: Valve, clock: Int): Array[Path] = {
    val targets = map.valves.filter(v => v.flowRate > 0 && !visited.contains(v))
    val paths = targets.map(v => map.shortestPath(from, v, clock))
    paths.sorted.reverse
  }

  def markVisited(valve: Valve, clock: Int): Unit = {
    if (clock <= Dec16Global.minutes) {
      visited(valve) = clock
    }
  }

  def markUnvisited(valve: Valve): Unit = {
    visited.remove(valve)
  }
}

case class ValveMap(valves: Array[Valve], verbose: Boolean = false) {

  def nonZero: Array[Valve] = {
    valves.filter(_.flowRate > 0)
  }

  def walk(part: Int = 1): Walk = {
    if (part == 1) {
      walkInner(Walk(this), lookup("AA"), 0)
    } else {
      walkInner2(Walk(this), lookup("AA"), lookup("AA"), 0, 0)
    }
  }

  def walkInner(walk: Walk, current: Valve, clock: Int): Walk = {
    // Need at least two minutes to move and turn on a valve
    if (walk.unvisitedCount > 0 && clock < Dec16Global.minutes - 2) {
      val targets = walk.paths(current, clock)
      if (verbose) {
        println("=== current " + current.name + " unvisited=" + walk.unvisitedCount + " ===")
        targets.foreach(println)
      }
      targets.foreach { nextPath =>
        val nextValve = nextPath.to
        val newClock = clock + nextPath.pathLength + 1
        if (newClock < Dec16Global.minutes) {
          walk.markVisited(nextValve, newClock)
          // recurse
          walkInner(walk, nextValve, newClock)
          walk.markUnvisited(nextValve)
        } else {
          if (walk.recordSolution()) {
            //println("TOTAL SHORT: " + walk.path + ": " + walk.toList.map(_.value).sum)
          }
        }
      }
    } else {
      if (walk.recordSolution()) {
        if (verbose) println("Walk: \n" + walk.toList.mkString("\n"))
        //println("TOTAL: " + walk.path + ": " + walk.toList.map(_.value).sum)
      }
    }
    walk
  }

  def walkInner2(walk: Walk, current1: Valve, current2: Valve, clock1: Int, clock2: Int): Walk = {
    // Need at least two minutes to move and turn on a valve
    if (walk.unvisitedCount > 0 && math.min(clock1, clock2) < Dec16Global.minutes - 2) {
      val targets = walk.paths(current1, clock1)
      val targets2 = walk.paths(current2, clock2)
      val all = targets.concat(targets2)
      val divide = all.splitAt(all.length / 2)
      // first half could be empty, so a temp fix, get path back to AA
      val one = if (divide._1.isEmpty) Array(shortestPath(current1, lookup("AA"), clock1)) else divide._1
      if (verbose) println("current1: "+ current1.name + " clock1: " + clock1 + " current2: " + current2.name +
      " clock2: " + clock2 + " divide1: " + divide._1.length + " divide2: "+ divide._2.length)
      one.foreach { nextPath1 =>
        divide._2.foreach { nextPath2 =>
          val nextValve1 = nextPath1.to
          val nextValve2 = nextPath2.to
          val newClock1 = clock1 + nextPath1.pathLength + 1
          val newClock2 = clock2 + nextPath2.pathLength + 1
          if (math.min(newClock1, newClock2) < Dec16Global.minutes) {
            walk.markVisited(nextValve1, newClock1)
            walk.markVisited(nextValve2, newClock2)
            // recurse
            walkInner2(walk, nextValve1, nextValve2, newClock1, newClock2)
            walk.markUnvisited(nextValve1)
            walk.markUnvisited(nextValve2)
          } else {
            if (walk.recordSolution()) {
              //println("TOTAL SHORT: " + walk.path + ": " + walk.toList.map(_.value).sum)
            }
          }
        }
      }
    } else {
      if (walk.recordSolution()) {
        if (verbose) println("Walk: \n" + walk.toList.mkString("\n"))
        //println("TOTAL: " + walk.path + ": " + walk.toList.map(_.value).sum)
      }
    }
    walk
  }

  val lookup: Map[String, Valve] = {
    val map = valves.map(v => v.name -> v).toMap
    valves.foreach{ v =>
      v.leadsToNames.foreach(name => v.addValve(map(name)))
    }
    map
  }

  val graph: Graph[Valve, Edge] = {
    val g = new DefaultUndirectedGraph[Valve, Edge](classOf[Edge])
    valves.foreach { valve =>
      g.addVertex(valve)
    }
    valves.foreach { valve =>
      valve.leadsToNames.foreach { to =>
        val toV = lookup(to)
        g.addEdge(valve, toV, Edge(valve, toV))
      }
    }
    g
  }

  def shortestPath(from: String, to: String, clock: Int): Path = {
    shortestPath(lookup(from), lookup(to), clock)
  }

  val shortest = new mutable.HashMap[String, GraphPath[Valve, Edge]]
  def shortestPathCached(from: Valve, to: Valve): GraphPath[Valve, Edge] = {
    val key = from.name + "--" + to.name
    shortest.get(key) match {
      case Some(p) => p
      case None =>
        val algo = new DijkstraShortestPath[Valve, Edge](graph)
        val p = algo.getPath(from, to)
        shortest(key) = p
        p
    }
  }
  def shortestPath(from: Valve, to: Valve, clock: Int): Path = {
    Path(from, to, shortestPathCached(from, to), clock)
  }

  // this was useful for generating data for a visualization tool
  // https://csacademy.com/app/graph_editor/
  def printAllPairs(): Unit = {
    valves.foreach { valve =>
      valve.leadsToNames.foreach { to =>
        println(valve.name + " " + to)
      }
    }
  }
}

case class Edge(from: Valve, to: Valve) {
  override def toString: String = from.name + "--" + to.name
}

case class Valve(name: String, flowRate: Int, leadsToNames: List[String]) {

  def totalFlow(openClock: Int): Int = (Dec16Global.minutes - openClock) * flowRate

  val leadsTo = new mutable.HashMap[String, Valve]

  def addValve(valve: Valve): Unit = {
    leadsTo(valve.name) = valve
  }

  override def toString: String = name + " [" + flowRate + "] => " + leadsToNames.mkString("|")
}

case class Path(from: Valve, to: Valve, path: GraphPath[Valve, Edge], clock: Int) extends Ordered[Path] {

  val value: Int = to.flowRate * (Dec16Global.minutes - clock - path.getLength - 1)

  val nodePath: Array[Valve] = path.getVertexList.asScala.toArray

  val pathLength: Int = path.getLength

  override def compare(that: Path): Int = {
    if (value == that.value) {
      that.pathLength - pathLength
    } else {
      value - that.value
    }
  }

  override def toString: String = {
    from.name + " -> " + to.name + ": " + nodePath.map(_.name).mkString(" => ") + " value=" + value
  }
}
