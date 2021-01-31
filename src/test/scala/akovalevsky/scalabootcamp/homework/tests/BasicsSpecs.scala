package akovalevsky.scalabootcamp.homework.tests

import akovalevsky.scalabootcamp.homework.Basics.{gcd, lcm}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.BigInt.int2bigInt

class BasicsSpecs extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "gcd" should "return the greatest common divisor's absolute value" in {
    forAll { (x: Int, y: Int) =>
      gcd(x, y) shouldEqual gcd(math.abs(x), math.abs(y))
    }
  }

  "gcd" should "return non-zero argument result if one of arguments is zero" in {
    forAll { x: Int =>
      gcd(x, 0) shouldEqual math.abs(x)
      gcd(0, x) shouldEqual math.abs(x)
    }
  }

  "gcd" should "return zero if both arguments are zeroes" in {
    gcd(0, 0) shouldEqual 0
  }

  "gcd" should "should work as scala.math.BigInt.gcd" in {
    forAll { (x: Int, y: Int) =>
      gcd(x, y) shouldEqual x.gcd(y)
    }
  }

  "lcm" should "return None if one/both of arguments are zeroes" in {
    forAll { x: Int =>
      lcm(x, 0) shouldEqual None
      lcm(0, x) shouldEqual None
    }
    lcm(0, 0) shouldEqual None
  }

  "lcm" should "return correct results for non-zero arguments" in {
    lcm(22, 22) shouldEqual Some(22)
    lcm(13, 17) shouldEqual Some(221)
    lcm(11, 3) shouldEqual Some(33)
    lcm(1, 100) shouldEqual Some(100)
    lcm(22, 33) shouldEqual Some(66)
  }
}