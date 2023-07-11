package com.donohoedigital.advent2022;

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class BoxTest {

  @Test
  def testBox(): Unit = {
    val b1 = Box(0, 1,1,1)
    val b2 = Box(1, 2,1,1)
    val shared = b1.sharesSide(b2)
    assertTrue(shared.nonEmpty)
    println("Shared: " + shared)
  }
}