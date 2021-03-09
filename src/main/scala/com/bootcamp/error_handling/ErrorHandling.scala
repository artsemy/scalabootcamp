package com.bootcamp.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandling {


  case class PaymentCard(name: String,
                         number: String,
                         expirationDate: String,
                         securityCode: String)

  sealed trait ValidationError
  object ValidationError {
    final case object OwnerNameLengthIsInvalid extends ValidationError {
      override def toString: String = "Ownername must be between 3 and 30 characters"
    }
    final case object NumberFormatIsInvalid extends ValidationError {
      override def toString: String = "Number must be number"
    }
    final case object ExpirationDateFormatIsInvalid extends ValidationError {
      override def toString: String = "Date must be date"
    }
    final case object SecurityCodeFormatIsInvalid extends ValidationError {
      override def toString: String = "Securitycode must be number"
    }
    final case object SecurityCodeLengthIsInvalid extends ValidationError {
      override def toString: String = "Securitycode must be 3 digits"
    }
  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = {
      (
        valdateOwnerName(name),
        validateNumberFormat(number),
        validateExpirationDateFormat(expirationDate),
        validateSecurityCode(securityCode)
        ).mapN(PaymentCard)
    }

    def valdateOwnerName(name: String): AllErrorsOr[String] =
      if (name.length > 3 && name.length < 20) name.validNec
      else OwnerNameLengthIsInvalid.invalidNec

    def validateNumberFormat(number: String): AllErrorsOr[String] =
      if (number.toIntOption.isDefined) number.validNec
      else NumberFormatIsInvalid.invalidNec

    def validateExpirationDateFormat(date: String): AllErrorsOr[String] =
      if (date.matches("((0[1-9])|(1[0-2])).(1|2)/d{3}")) date.validNec
      else ExpirationDateFormatIsInvalid.invalidNec

    def validateSecurityCode(code: String): AllErrorsOr[String] = {

      def validateSecurityCodeFormat(): AllErrorsOr[String] = {
        if (code.toIntOption.isDefined) code.validNec
        else SecurityCodeFormatIsInvalid.invalidNec
      }

      def validateSecurityCodeLength(): AllErrorsOr[String] = {
        if (code.length == 3) code.validNec
        else SecurityCodeLengthIsInvalid.invalidNec
      }

      validateSecurityCodeFormat().andThen(_ => validateSecurityCodeLength())
    }
  }
}
