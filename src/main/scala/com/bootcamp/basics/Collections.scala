package com.bootcamp.basics

import scala.annotation.tailrec

object Collections {

  // hometask:
  // https://leetcode.com/problems/running-sum-of-1d-array/
  // https://leetcode.com/problems/shuffle-the-array
  // https://leetcode.com/problems/richest-customer-wealth
  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points

  def runningSum(nums: Array[Int]): Array[Int] = {
    @tailrec
    def sum(x: Int, array: Array[Int]) {
      if (array.nonEmpty) {
        array.update(0, array.head + x)
        sum(array.head, array.tail)
      }
    }
    sum(nums.head, nums.tail)
    nums
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
//    val arr1: Array[Int] = nums.take(n)
//    val arr2: Array[Int] = nums.takeRight(n)
    val arr1 = new Array[Int](n)
    nums.copyToArray(arr1, 0, n)
    val arr2 = new Array[Int](n)
    nums.copyToArray(arr2, 0)
    arr1.zip(arr2).flatMap(x => Array(x._1, x._2))
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.reduce((a, b) => if (a.sum > b.sum) a else b).sum
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max: Int = candies.max
    candies.map(x => x + extraCandies >= max)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val l1 = points.map(x => x.head)
    l1.max
  }

  def main(args: Array[String]): Unit = {
//    println(runningSum(Array(1, 2, 3, 4)).mkString("[", ",", "]"))
//    println(shuffle(Array(1, 2, 3, 4), 2).mkString("[", ",", "]"))
//    println(maximumWealth(Array(Array(1, 5), Array(7, 3), Array(3, 5))))
//    println(kidsWithCandies(Array(2, 3, 5, 1, 3), 3).mkString("[", ",", "]"))
  }
}
