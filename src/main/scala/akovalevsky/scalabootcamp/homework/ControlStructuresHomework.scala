package akovalevsky.scalabootcamp.homework

import akovalevsky.scalabootcamp.homework.ControlStructuresHomework.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source

object ControlStructuresHomework {

  sealed trait Command

  object Command {

    final case class Divide private(dividend: Double, divisor: Double) extends Command

    object Divide {
      def apply(dividend: Double, divisor: Double): Either[ErrorMessage, Divide] = {
        if (divisor == 0)
          Left(ErrorMessage("The divisor can't be zero"))
        else
          Right(new Divide(dividend, divisor))
      }
    }

    final case class Sum private(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    object Average {
      def apply(numbers: List[Double]): Either[ErrorMessage, Average] = {
        if (numbers.isEmpty)
          Left(ErrorMessage("The average command requires at least 1 argument"))
        else
          Right(new Average(numbers))
      }
    }

    final case class Min(numbers: List[Double]) extends Command

    object Min {
      def apply(numbers: List[Double]): Either[ErrorMessage, Min] = {
        if (numbers.isEmpty)
          Left(ErrorMessage("The min command requires at least 1 argument"))
        else
          Right(new Min(numbers))
      }
    }

    final case class Max(numbers: List[Double]) extends Command

    object Max {
      def apply(numbers: List[Double]): Either[ErrorMessage, Max] = {
        if (numbers.isEmpty)
          Left(ErrorMessage("The max command requires at least 1 argument"))
        else
          Right(new Max(numbers))
      }
    }

  }

  final case class ErrorMessage(value: String)

  final case class Result[+A <: Command](command: A, value: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    def parseArgs(args: List[String]): Either[ErrorMessage, List[Double]] = {
      val argsParsed = args.map(_.toDoubleOption)
      if (argsParsed.forall(!_.isEmpty))
        Right(argsParsed.map(_.get))
      else
        Left(ErrorMessage("Failed to parse arguments"))
    }

    x.trim.toLowerCase.split("\\s+").toList match {
      case "divide" :: args if args.length != 2 => Left(ErrorMessage("The divide command requires exactly 2 arguments"))
      case "divide" :: args => for {
        parsedArgs <- parseArgs(args)
        command <- Divide(parsedArgs(0), parsedArgs(1))
      } yield command
      case "sum" :: args => for {
        parsedArgs <- parseArgs(args)
      } yield Sum(parsedArgs)
      case "average" :: args => for {
        parsedArgs <- parseArgs(args)
        command <- Average(parsedArgs)
      } yield command
      case "min" :: args => for {
        parsedArgs <- parseArgs(args)
        command <- Min(parsedArgs)
      } yield command
      case "max" :: args => for {
        parsedArgs <- parseArgs(args)
        command <- Max(parsedArgs)
      } yield command
      case _ => Left(ErrorMessage("The command is unknown"))
    }
  }

  // we are done with various validations during the parse phase,
  // so we can just return the Result[Command] instead of Either[ErrorMessage, Result[Command]]
  def calculate(x: Command): Result[Command] = {
    x match {
      case Divide(dividend, divisor) => Result(x, dividend / divisor)
      case Sum(numbers) => Result(x, numbers.sum)
      case Average(numbers) => Result(x, numbers.sum / numbers.length)
      case Min(numbers) => Result(x, numbers.min)
      case Max(numbers) => Result(x, numbers.max)
    }
  }

  def renderResult(x: Result[Command]): String = {
    x match {
      case Result(Divide(dividend, divisor), value) => f"$dividend divided by $divisor is $value%.4f"
      case Result(Sum(Nil), value) => s"the sum of zero numbers is $value"
      case Result(Sum(numbers), value) => s"the sum of ${numbers.mkString(" ")} is $value"
      case Result(Average(numbers), value) => f"the average of ${numbers.mkString(" ")} is $value%.4f"
      case Result(Min(numbers), value) => s"the minimum of ${numbers.mkString(" ")} is $value"
      case Result(Max(numbers), value) => s"the maximum of ${numbers.mkString(" ")} is $value"
    }
  }

  def process(x: String): String = {
    val result = for {
      command <- parseCommand(x)
    } yield calculate(command)

    result.fold(err => s"Error: ${err.value}", renderResult)
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
