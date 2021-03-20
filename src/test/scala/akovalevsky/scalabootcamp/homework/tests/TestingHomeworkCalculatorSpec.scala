package akovalevsky.scalabootcamp.homework.tests

import akovalevsky.scalabootcamp.homework.TestingHomework.{Calculator, CalculatorError, Operation}
import org.scalatest.Inside.inside
import org.scalatest.funsuite.AnyFunSuite

class TestingHomeworkCalculatorSpec extends AnyFunSuite {

  object Fixture {
    def calculatorWithDigitsEntered: Calculator = Calculator(screen = 1234)

    def calculatorWithPendingPlusOperation: Calculator = Calculator(memory = 1234, screen = 1, Some(Operation.Plus))

    def emptyCalculator: Calculator = Calculator()

    def doNTimes(calculator: Calculator, f: Calculator => Either[CalculatorError, Calculator], n: Int): Either[CalculatorError, Calculator] = {
      def go(calculator: Either[CalculatorError, Calculator], f: Calculator => Either[CalculatorError, Calculator], n: Int): Either[CalculatorError, Calculator] =
        if (n == 0) calculator
        else calculator match {
          case Left(_) => calculator
          case Right(c) => go(f(c), f, n - 1)
        }

      go(Right(calculator), f, n)
    }

  }

  test("Just created calculator has valid default state") {
    val calculator = Fixture.emptyCalculator

    assert(calculator == Calculator(0, 0, None), "The constructor sets invalid values")
  }

  test("Entering non-digit produces relevant error") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = calculator.enter(25)

    assert(resultCalculator == Left(CalculatorError.InvalidDigit), "The method doesn't validate the digit entered")
  }

  test("Division by zero produces relevant error") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(1)
      c <- c.enter(2)
      c <- c.divide
      c <- c.enter(0)
      c <- c.calculate
    } yield c

    assert(resultCalculator == Left(CalculatorError.DivisionByZero))
  }

  test("Entering digits displays the number consisting of digits on the screen.") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(1)
      c <- c.enter(2)
      c <- c.enter(3)
      c <- c.enter(4)
    } yield c

    assert(resultCalculator == Right(Calculator(0, screen = 1234, None)), "The method calculates the screen value wrong")
  }

  test("Pressing any operation should perform the current operation and copy the result to the screen") {
    val calculatorWithPendingPlusOp = Fixture.calculatorWithPendingPlusOperation
    val plusOpResult = calculatorWithPendingPlusOp.screen + calculatorWithPendingPlusOp.memory

    inside(calculatorWithPendingPlusOp.plus) {
      case Right(x) => assert(x.screen == plusOpResult)
    }

    inside(calculatorWithPendingPlusOp.minus) {
      case Right(x) => assert(x.screen == plusOpResult)
    }

    inside(calculatorWithPendingPlusOp.multiply) {
      case Right(x) => assert(x.screen == plusOpResult)
    }

    inside(calculatorWithPendingPlusOp.divide) {
      case Right(x) => assert(x.screen == plusOpResult)
    }
  }

  test("Pressing any operation should clean the memory") {
    val calculatorWithPendingPlusOp = Fixture.calculatorWithPendingPlusOperation

    inside(calculatorWithPendingPlusOp.plus) {
      case Right(x) => assert(x.memory == 0)
    }

    inside(calculatorWithPendingPlusOp.minus) {
      case Right(x) => assert(x.memory == 0)
    }

    inside(calculatorWithPendingPlusOp.multiply) {
      case Right(x) => assert(x.memory == 0)
    }

    inside(calculatorWithPendingPlusOp.divide) {
      case Right(x) => assert(x.memory == 0)
    }
  }

  test("Pressing any operation more than one time changes nothing") {
    import Fixture._

    val calculator = calculatorWithDigitsEntered
    val clue = "Pressing operation more than one time changes the state"

    assert(doNTimes(calculator, _.plus, 1) == doNTimes(calculator, _.plus, 10), clue)
    assert(doNTimes(calculator, _.minus, 1) == doNTimes(calculator, _.minus, 10), clue)
    assert(doNTimes(calculator, _.multiply, 1) == doNTimes(calculator, _.multiply, 10), clue)
    assert(doNTimes(calculator, _.divide, 1) == doNTimes(calculator, _.divide, 10), clue)
    assert(doNTimes(calculator, _.calculate, 1) == doNTimes(calculator, _.calculate, 10), clue)
  }

  test("Sum calculation is right") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(1)
      c <- c.enter(2)
      c <- c.plus
      c <- c.enter(3)
      c <- c.enter(4)
      c <- c.calculate
    } yield c

    assert(resultCalculator == Right(Calculator(memory = 0, screen = 46)))
  }

  test("Product calculation is right") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(1)
      c <- c.enter(2)
      c <- c.multiply
      c <- c.enter(3)
      c <- c.enter(4)
      c <- c.calculate
    } yield c

    assert(resultCalculator == Right(Calculator(memory = 0, screen = 408)))
  }

  test("Difference calculation is right") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(1)
      c <- c.enter(2)
      c <- c.minus
      c <- c.enter(3)
      c <- c.enter(4)
      c <- c.calculate
    } yield c

    assert(resultCalculator == Right(Calculator(memory = 0, screen = -22)))
  }

  test("Division produces the integer part of the division of the result") {
    val calculator = Fixture.emptyCalculator

    val resultCalculator = for {
      c <- calculator.enter(2)
      c <- c.enter(2)
      c <- c.divide
      c <- c.enter(3)
      c <- c.calculate
    } yield c

    assert(resultCalculator == Right(Calculator(memory = 0, screen = 7)))
  }

  test("Several operations in a row are right") {
    val calculator = Fixture.emptyCalculator

    val firstResultCalculator = for {
      c <- calculator.minus
      c <- c.enter(2)
      c <- c.plus
      c <- c.enter(3)
      c <- c.enter(2)
      c <- c.divide
      c <- c.enter(1)
      c <- c.enter(5)
      c <- c.calculate
    } yield c

    assert(firstResultCalculator == Right(Calculator(memory = 0, screen = 2)))

    val secondResultCalculator = for {
      c <- firstResultCalculator
      c <- Right(c.reset)
      c <- c.enter(1)
      c <- c.plus
      c <- c.enter(1)
      c <- c.plus
      c <- c.multiply
      c <- c.minus
      c <- c.enter(2)
      c <- c.calculate
    } yield c

    assert(secondResultCalculator == Right(Calculator(memory = 0, screen = 0)))
  }

}
