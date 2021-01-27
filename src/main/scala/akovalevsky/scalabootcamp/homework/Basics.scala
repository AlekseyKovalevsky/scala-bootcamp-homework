package akovalevsky.scalabootcamp.homework

object Basics {
  def lcm(a: Int, b: Int): Option[Int] = {
    if (a == 0 || b == 0)
      None
    else
      Some((a / gcd(a, b)) * b)
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
