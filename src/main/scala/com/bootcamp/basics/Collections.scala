package com.bootcamp.basics

object Collections {

  // hometask:
  // https://leetcode.com/problems/running-sum-of-1d-array/
  // https://leetcode.com/problems/shuffle-the-array
  // https://leetcode.com/problems/richest-customer-wealth
  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points

  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scan(0)(_ + _).tail
  }

  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    val arr1 = nums.dropRight(n)
    val arr2 = nums.drop(n)
    arr1.zip(arr2).flatMap(x => Array(x._1, x._2)) //fix
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.reduce((a, b) => if (a.sum > b.sum) a else b).sum
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max: Int = candies.max
    candies.map(x => x + extraCandies >= max)
  }

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val l1: Array[Int] = points.map(x => x.head)
    val l2 = l1.sorted
    def func(array: Array[Int]): Int = {
      if (array.length > 2) {
        math.max(array.apply(1) - array.head, func(array.tail))
      }
      else {
        array.apply(1) - array.head
      }
    }
    func(l2)
  }

  def main(args: Array[String]): Unit = {
    println(runningSum(Array(1, 2, 3, 4)).mkString("[", ",", "]")) //done
    println(shuffle(Array(1, 2, 3, 4), 2).mkString("[", ",", "]")) //done
    println(maximumWealth(Array(Array(1, 5), Array(7, 3), Array(3, 5)))) //done
    println(kidsWithCandies(Array(2, 3, 5, 1, 3), 3).mkString("[", ",", "]")) //done
    println(maxWidthOfVerticalArea(Array(Array(8, 7), Array(9, 9), Array(7, 4), Array(9, 7)))) //done
  }

}
