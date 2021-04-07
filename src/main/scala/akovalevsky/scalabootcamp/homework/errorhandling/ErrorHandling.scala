package akovalevsky.scalabootcamp.homework.errorhandling

import akovalevsky.scalabootcamp.homework.errorhandling.ErrorHandling.ValidationError._
import org.joda.time.LocalDate
import cats.syntax.all._
import cats.Show

object ErrorHandling {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

  case class PaymentCard private(name: String,
                                 number: Seq[Int],
                                 expirationDate: LocalDate,
                                 securityCode: Seq[Int])

  object PaymentCard {

    import akovalevsky.scalabootcamp.homework.errorhandling.ErrorHandling.PaymentCardValidator._

    private def validate(
                          name: String,
                          number: String,
                          expirationDate: String,
                          securityCode: String,
                        ): AllErrorsOr[PaymentCard] =
      (validateName(name), validateNumber(number), validateExpDate(expirationDate), validateSecurityCode(securityCode))
        .mapN {
          case (nameValidated, numberValidated, expDateValidated, securityCodeValidated) =>
            new PaymentCard(nameValidated, numberValidated, expDateValidated, securityCodeValidated)
        }

    def apply(name: String,
              number: String,
              expirationDate: String,
              securityCode: String): AllErrorsOr[PaymentCard] =
      validate(name, number, expirationDate, securityCode)
  }

  sealed trait ValidationError

  object ValidationError {

    final case object NumberIsNotNumeric extends ValidationError {
      override def toString: String = "Number must contain only digits"
    }

    final case object NumberLengthIsInvalid extends ValidationError {
      override def toString: String = "Number length should be 16"
    }

    final case object NumberChecksumIsInvalid extends ValidationError {
      override def toString: String = "Number is not valid credit card number"
    }

    final case object NameFormatIsInvalid extends ValidationError {
      override def toString: String = "Name should contain uppercase name and surname separated by space, composed of A-Z letters"
    }

    final case object NameLengthIsInvalid extends ValidationError {
      override def toString: String = "Name should contain 2 to 26 symbols"
    }

    final case object ExpDateIsNotDate extends ValidationError {
      override def toString: String = "Expiration date must contain valid month and last two digits of the year separated by '/'"
    }

    final case object ExpDateHasPassed extends ValidationError {
      override def toString: String = "Expiration date has passed"
    }

    final case object SecurityCodeIsNotNumeric extends ValidationError {
      override def toString: String = "Security code must contain only digits"
    }

    final case object SecurityCodeLengthIsInvalid extends ValidationError {
      override def toString: String = "Security code length must be 3"
    }

  }

  object PaymentCardValidator {

    import cats.data.ValidatedNec

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validateIsNumericWithErr(err: ValidationError): String => AllErrorsOr[Seq[Int]] = (x: String) => {
      if (x.forall(_.isDigit)) x.map(_.asDigit).valid
      else err.invalidNec
    }


    def validateNumberIsNumeric: String => AllErrorsOr[Seq[Int]] = validateIsNumericWithErr(NumberIsNotNumeric)

    def validateNumberLength(x: Seq[Int]): AllErrorsOr[Seq[Int]] =
      if (x.length == 16) x.validNec
      else NumberLengthIsInvalid.invalidNec

    def validateNumberChecksum(x: Seq[Int]): AllErrorsOr[Seq[Int]] = {
      def checkLuhn(x: Seq[Int]): Boolean = {
        val checkDigit = x.last
        val digitsExceptLast = x.dropRight(1)
        val oddIdxDigits = digitsExceptLast.zipWithIndex.filterNot { case (_, idx) => idx % 2 == 0 }.map(_._1)
        val evenIdxDigits = digitsExceptLast.zipWithIndex.filter { case (_, idx) => idx % 2 == 0 }.map(_._1)

        ((oddIdxDigits.map { x => if (x * 2 > 9) x * 2 - 9 else x }.sum + evenIdxDigits.sum + checkDigit) * 9) % 10 == checkDigit
      }

      if (checkLuhn(x)) x.validNec
      else NumberChecksumIsInvalid.invalidNec
    }

    def validateNumber(x: String): AllErrorsOr[Seq[Int]] =
      validateNumberIsNumeric(x)
        .andThen(validateNumberLength)
        .andThen(validateNumberChecksum)

    def validateNameFormat(x: String): AllErrorsOr[String] =
      if (x.matches("[A-Z]+\\s[A-Z]+")) x.validNec
      else NameFormatIsInvalid.invalidNec

    def validateNameLength(x: String): AllErrorsOr[String] =
      if (x.length >= 2 && x.length <= 26) x.validNec
      else NameLengthIsInvalid.invalidNec

    def validateName(x: String): AllErrorsOr[String] = validateNameFormat(x) *> validateNameLength(x)

    def validateExpDateIsDate(x: String): AllErrorsOr[LocalDate] = {
      val expDateMatch = "^(0[1-9]|1[0-2])/([0-9][0-9])$".r.findFirstMatchIn(x)

      expDateMatch.fold(ExpDateIsNotDate.invalidNec: AllErrorsOr[LocalDate])(
        x => LocalDate.now()
          .withYearOfCentury(x.group(2).replaceFirst("^0(?!$)", "").toInt)
          .withMonthOfYear(x.group(1).replaceFirst("^0(?!$)", "").toInt)
          .withDayOfMonth(1)
          .validNec
      )
    }

    def validateExpDateHasNotPassed(x: LocalDate, thisMonth: => LocalDate): AllErrorsOr[LocalDate] = {
      if (x.isAfter(thisMonth)) x.validNec
      else ExpDateHasPassed.invalidNec
    }

    def validateExpDate(x: String): AllErrorsOr[LocalDate] = validateExpDateIsDate(x)
      .andThen(expDate => validateExpDateHasNotPassed(expDate, LocalDate.now().withDayOfMonth(1)))

    def validateSecurityCodeIsNumeric: String => AllErrorsOr[Seq[Int]] = validateIsNumericWithErr(SecurityCodeIsNotNumeric)

    def validateSecurityCodeLength(x: String): AllErrorsOr[String] =
      if (x.length == 3) x.validNec
      else SecurityCodeLengthIsInvalid.invalidNec

    def validateSecurityCode(x: String): AllErrorsOr[Seq[Int]] =
      validateSecurityCodeLength(x) *> validateSecurityCodeIsNumeric(x)
  }

  def main(args: Array[String]): Unit = {
    val validCard = PaymentCard("HELLO WORLD", "4111111111111111", "02/50", "321")
    val invalidNameFormatCard = PaymentCard("Hello", "4111111111111111", "02/22", "321")
    val invalidNumberCard = PaymentCard("HELLO WORLD", "5111111111111111", "02/22", "321")
    val invalidDateFormatCard = PaymentCard("HELLO WORLD", "4111111111111111", "33/23", "321")
    val expiredDateCard = PaymentCard("HELLO WORLD", "4111111111111111", "02/20", "321")
    val invalidSecurityCodeFormatCard = PaymentCard("HELLO WORLD", "4111111111111111", "02/20", "abcd")
    val invalidAllCard = PaymentCard("Hello", "5111111111111111", "55/22", "abcd")

    implicit val showForValidationError: Show[ValidationError] = (x: ValidationError) => x.toString

    Seq(validCard, invalidNameFormatCard, invalidNumberCard, invalidDateFormatCard,
      expiredDateCard, invalidSecurityCodeFormatCard, invalidAllCard)
      .map {
        x => x.fold(errors => s"Invalid data: ${errors.toList.mkString("; ")}", card => card.toString)
      } foreach println
  }

}
