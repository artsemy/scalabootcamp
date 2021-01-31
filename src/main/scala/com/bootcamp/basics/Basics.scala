package com.bootcamp.basics

import scala.annotation.tailrec

object Basics {

  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Least_common_multiple and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def lcm(a: Int, b: Int): Int = {
    math.abs(a*b) / gcd(a, b)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a == 0) b else if (b == 0) a else
    if (a >= b) gcd (a-b, b) else gcd(b, a)
  }

}
