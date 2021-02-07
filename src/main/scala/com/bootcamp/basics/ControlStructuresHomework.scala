package com.bootcamp.basics

import com.bootcamp.basics.ControlStructuresHomework.Command.{Average, Divide, Max, Min, Sum}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  val ErrorMessageStartLine = "Error: "
  val Space = "\\s+"

  final case class ErrorMessage(value: String) {
    val message = ErrorMessageStartLine.concat(value)
  }

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  sealed trait Result
  final case class ChangeMe(value: String) extends Result

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split(Space).toList match {
      case "divide" :: rest if rest.length == 2 => check(rest) match {
        case Success(value) if value.apply(1) != 0 => Right(Divide(value.head, value.apply(1)))
        case Failure(exception) => Left(ErrorMessage(exception.toString))
      }
      case "sum" :: rest => check(rest) match {
        case Success(value) => Right(Sum(value))
        case Failure(exception) => Left(ErrorMessage(exception.toString))
      }
      case "average" :: rest if rest.nonEmpty => check(rest) match {
        case Success(value) => Right(Average(value))
        case Failure(exception) => Left(ErrorMessage(exception.toString))
      }
      case "min" :: rest if rest.nonEmpty => check(rest) match {
        case Success(value) => Right(Min(value))
        case Failure(exception) => Left(ErrorMessage(exception.toString))
      }
      case "max" :: rest if rest.nonEmpty => check(rest) match {
        case Success(value) => Right(Max(value))
        case Failure(exception) => Left(ErrorMessage(exception.toString))
      }
      case _ => Left(ErrorMessage("bad input"))
    }
  }

  def check(list: List[String]): Try[List[Double]] = {
    Try(list.map(x => x.toDouble))
  }

  def calculate(x: Command): Double = x match{
    case Divide(dividend, divisor) => dividend / divisor
    case Sum(numbers) => numbers.sum
    case Average(numbers) => numbers.sum / numbers.length
    case Max(numbers) => numbers.max
    case Min(numbers) => numbers.min
    case _ => 0
  }

  def renderResult(x: Command): String = x match {
    case Divide(dividend, divisor) => s"$dividend divided by $divisor is ${calculate(x)}"
    case Sum(numbers) => s"the sum of ${numbers.mkString(" ")} is ${calculate(x)}"
    case Average(numbers) => s"the average of ${numbers.mkString(" ")} is ${calculate(x)}"
    case Min(numbers) => s"the minimum of ${numbers.mkString(" ")} is ${calculate(x)}"
    case Max(numbers) => s"the maximum of ${numbers.mkString(" ")} is ${calculate(x)}"
  }

  def process(x: String): String = {
    parseCommand(x) match {
      case Right(value) => renderResult(value)
      case Left(value) => value.message
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println

}
