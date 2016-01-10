package com.github.mdelem.scalaquest.engine

import javax.xml.bind.ValidationException
import com.github.nscala_time.time.Imports._

object Validator {

  trait Validates[T] {
    def validate(input: String): T
  }

  case class ValidatesInt(min: Int, max: Int, step: Int) extends Validates[Int] {
    def validate(input: String): Int = {
      val x = wrapException(()=>input.toInt)
      if (x < min || x > max || x % step != 0)
        throw new ValidationException(s"Invalid value: $x; Must be between ${min} and ${max} and divisible by ${step}")
      else
        x
    }
  }
  
  case class ValidatesLong(min: Long, max: Long, step: Long) extends Validates[Long] {
    def validate(input: String): Long = {
      val x = wrapException(()=>input.toLong)
      if (x < min || x > max || x % step != 0)
        throw new ValidationException(s"Invalid value: $x; Must be between ${min} and ${max} and divisible by ${step}")
      else
        x
    }
  }
  
  case class ValidatesDouble(min: Double, max: Double) extends Validates[Double] {
    def validate(input: String): Double = {
      val x = wrapException(()=>input.toDouble)
      if (x < min || x > max)
        throw new ValidationException(s"Invalid value: $x; Must be between ${min} and ${max}")
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
        wrapException(()=>input.toInt)
      }
    }
    implicit object ValidatesLong extends Validates[Long] {
      def validate(input: String): Long = {
        wrapException(()=>input.toLong)
      }
    }
    implicit object ValidatesDouble extends Validates[Double] {
      def validate(input: String): Double = {
        wrapException(()=>input.toDouble)
      }
    }
    implicit object ValidatesBoolean extends Validates[Boolean] {
      def validate(input: String): Boolean = {
        wrapException(()=>input.toBoolean)
      }
    }
    implicit object ValidatesString extends Validates[String] {
      def validate(input: String): String = {
        input
      }
    }
    implicit object ValidatesDateTime extends Validates[DateTime] {
      def validate(input: String): DateTime = {
        wrapException(()=>DateTime.parse(input))
      }
    }
  }
  
  private def wrapException[T](call: () => T) = {
    try {
      call()
    } catch {
      case e:NumberFormatException => throw new ValidationException(e)
      case e:IllegalArgumentException => throw new ValidationException(e)
    }
  }

}