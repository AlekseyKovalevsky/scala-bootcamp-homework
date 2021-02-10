package akovalevsky.scalabootcamp.homework

import scala.annotation.tailrec

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array

  def runningSum(nums: Array[Int]): Array[Int] = nums.scanLeft(0)(_ + _).tail

  def runningSumWithFold(nums: Array[Int]): Array[Int] =
    nums.foldLeft(Array[Int](0)) { (runningSum, next) => runningSum :+ (runningSum.last + next) }.tail

  // https://leetcode.com/problems/shuffle-the-array

  def shuffle(nums: Array[Int], n: Int): Array[Int] =
    nums.take(n).zip(nums.takeRight(n)).flatMap { case (a, b) => List(a, b) }

  // https://leetcode.com/problems/richest-customer-wealth

  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] =
    candies.map(candies.max <= _ + extraCandies)

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points

  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val xsSorted = points.map(_ (0)).sorted
    xsSorted.zip(xsSorted.tail).map { case (x1, x2) => math.abs(x1 - x2) }.max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/

  def maxDepth(s: String): Int = {
    val depthLog = s.scanLeft(0) { (currDepth, character) =>
      character match {
        case '(' => currDepth + 1
        case ')' => currDepth - 1
        case _ => currDepth
      }
    }

    depthLog.max
  }

  // https://leetcode.com/problems/split-a-string-in-balanced-strings

  def balancedStringSplit(s: String): Int = {
    val charCountLog = s.scanLeft((0, 0)) { (counts, currentChar) =>
      (counts, currentChar) match {
        case ((lCount, rCount), 'L') => (lCount + 1, rCount)
        case ((lCount, rCount), 'R') => (lCount, rCount + 1)
        case (_, _) => counts
      }
    }

    charCountLog.tail.count { case (lCount, rCount) => lCount == rCount }
  }


  def findGap(l: List[Int]): Option[(Int, Int)] =
    if (l.length < 2)
      None
    else l.zip(l.tail).find { case (x, y) => math.abs(x - y) > 1 }

  @tailrec
  def findGapRecursive(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil => None
    case x :: y :: _ if y - x > 1 => Some(x, y)
    case _ :: y => findGapRecursive(y)
  }

  def min(list: List[Int]): Option[Int] = {
    list match {
      case Nil => None
      case _ => Some(list.foldLeft(Int.MaxValue) { (minVal, x) => minVal min x })
    }
  }

  def minRecursive(list: List[Int]): Option[Int] = {
    @tailrec
    def go(list: List[Int], currentMin: Int): Option[Int] = list match {
      case Nil => Some(currentMin)
      case head :: tail => go(tail, math.min(currentMin, head))
    }

    if (list.isEmpty)
      None
    else go(list, Int.MaxValue)
  }

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] =
    list.foldLeft(List(zero)) { (results, next) => results :+ f(results.last, next) }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview

  def count(str: String): List[(Char, Int)] = {
    @tailrec
    def go(str: List[Char], counts: List[(Char, Int)]): List[(Char, Int)] = {
      if (str.isEmpty) {
        counts
      }
      else {
        val countsNew = (str, counts) match {
          case (char :: _, (c, charCount) :: charCounts) if char == c => (char, charCount + 1) :: charCounts
          case (char :: _, _) => (char, 1) :: counts
        }
        go(str.tail, countsNew)
      }
    }

    go(str.toCharArray.toList, Nil)
  }
}
