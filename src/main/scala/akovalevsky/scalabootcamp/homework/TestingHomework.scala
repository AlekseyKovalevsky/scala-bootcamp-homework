package akovalevsky.scalabootcamp.homework

import akovalevsky.scalabootcamp.homework.TestingHomework.CalculatorError.{DivisionByZero, InvalidDigit}

object TestingHomework {

  sealed trait Operation

  object Operation {

    case object Plus extends Operation

    case object Minus extends Operation

    case object Divide extends Operation

    case object Multiply extends Operation

  }

  sealed trait CalculatorError

  object CalculatorError {

    object DivisionByZero extends CalculatorError {
      override def toString: String = "Division by zero is not allowed"
    }

    object InvalidDigit extends CalculatorError {
      override def toString: String = "Only digits supported"
    }

  }


  // emulates simple classic calculator
  case class Calculator(
                         memory: Int = 0,
                         screen: Int = 0,
                         operation: Option[Operation] = None,
                         pendingSecondArg: Boolean = false) {
    private def calculateLastOperation: Either[CalculatorError, Int] = operation match {
      case None => Right(screen)
      case Some(Operation.Plus) => Right(memory + screen)
      case Some(Operation.Minus) => Right(memory - screen)
      case Some(Operation.Multiply) => Right(memory * screen)
      case Some(Operation.Divide) if screen == 0 => Left(DivisionByZero)
      case Some(Operation.Divide) => Right(memory / screen)
    }

    private def enterOperation(op: Operation): Either[CalculatorError, Calculator] = {
      if (pendingSecondArg)
        Right(this.copy(operation = Some(op)))
      else
        for {
          lastOpCalcResult <- calculateLastOperation
        } yield this.copy(memory = 0, screen = lastOpCalcResult, operation = Some(op), pendingSecondArg = true)
    }

    def enter(digit: Int): Either[CalculatorError, Calculator] = {
      if (digit >= 0 && digit <= 9) {
        if (pendingSecondArg) Right(this.copy(memory = screen, screen = digit, pendingSecondArg = false))
        else Right(this.copy(screen = screen * 10 + digit))
      }
      else Left(CalculatorError.InvalidDigit)
    }

    def plus: Either[CalculatorError, Calculator] = enterOperation(Operation.Plus)

    def minus: Either[CalculatorError, Calculator] = enterOperation(Operation.Minus)

    def multiply: Either[CalculatorError, Calculator] = enterOperation(Operation.Multiply)

    def divide: Either[CalculatorError, Calculator] = enterOperation(Operation.Divide)

    def calculate: Either[CalculatorError, Calculator] = for {
      lastOpCalcResult <- calculateLastOperation
    } yield this.copy(memory = 0, screen = lastOpCalcResult, None)

    def reset: Calculator = Calculator()

  }

}
