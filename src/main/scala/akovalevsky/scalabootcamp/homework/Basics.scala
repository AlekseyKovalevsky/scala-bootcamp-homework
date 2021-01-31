package akovalevsky.scalabootcamp.homework

import scala.annotation.tailrec

// use BigInt instead of Int to forget about Int overflows possible while calculating lcm or gcd
// Int overflow can happen during gcd calculation: gcd(Int.MinValue, 0) = Int.MinValue.abs == Int.MaxValue + 1 -> overflow
object Basics {
  def lcm(a: BigInt, b: BigInt): Option[BigInt] = {
    if (a == 0 || b == 0)
      None
    else
      Some(a.abs / gcd(a, b) * b.abs)
  }

  def gcd(a: BigInt, b: BigInt): BigInt = {
    @tailrec
    def gcdInternal(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcdInternal(b, a % b)

    gcdInternal(a.abs, b.abs)
  }
}