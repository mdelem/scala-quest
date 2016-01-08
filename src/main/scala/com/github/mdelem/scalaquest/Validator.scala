package com.github.mdelem.scalaquest

import javax.xml.bind.ValidationException
import scala.Double
import com.github.nscala_time.time.Imports._

object Validator {

  trait Validates[T] {
    def validate(input: String): T
  }

  case class ValidatesInt(min: Int, max: Int, step: Int) extends Validates[Int] {
    def validate(input: String): Int = {
      val x = input.toInt
      if (x < min || x > max || x % step != 0)
        throw new ValidationException(s"Invalid value: $x; Must be between ${min} and ${max} and divisible by ${step}")
      else
        x
    }
  }

  case class ValidatesString(values: Set[String]) extends Validates[String] {
    def validate(input: String): String = {
      if (values.contains(input))
        input
      else
        throw new ValidationException(s"Invalid value: $input; Must be one of: ${values}")
    }
  }

  object Validates {
    implicit object ValidatesInt extends Validates[Int] {
      def validate(input: String): Int = {
        input.toInt
      }
    }
    implicit object ValidatesLong extends Validates[Long] {
      def validate(input: String): Long = {
        input.toLong
      }
    }
    implicit object ValidatesDouble extends Validates[Double] {
      def validate(input: String): Double = {
        input.toDouble
      }
    }
    implicit object ValidatesBoolean extends Validates[Boolean] {
      def validate(input: String): Boolean = {
        input.toBoolean
      }
    }
    implicit object ValidatesString extends Validates[String] {
      def validate(input: String): String = {
        input
      }
    }
    implicit object ValidatesDate extends Validates[DateTime] {
      def validate(input: String): DateTime = {
        DateTime.parse(input)
      }
    }
  }

}