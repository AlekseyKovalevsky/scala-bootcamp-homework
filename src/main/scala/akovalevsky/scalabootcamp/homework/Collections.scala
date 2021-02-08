package akovalevsky.scalabootcamp.homework

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array
  def runningSum(nums: Array[Int]): Array[Int] = nums.scanLeft(0)(_ + _).tail

  def runningSumWithFold(nums: Array[Int]): Array[Int] =
    nums.foldLeft(Array[Int](0)) { (runningSum, next) => runningSum :+ (runningSum.last + next) }.tail

  // https://leetcode.com/problems/shuffle-the-array
  // how to 'unpack' tuple being flatMap argument?
  def shuffle(nums: Array[Int], n: Int): Array[Int] = nums.take(n).zip(nums.takeRight(n)).flatMap(_ match { case (a, b) => List(a, b) })

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.map(_.sum).max

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = candies.map(candies.max <= _ + extraCandies)
}
