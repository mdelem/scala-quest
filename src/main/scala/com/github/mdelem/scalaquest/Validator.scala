package com.github.mdelem.scalaquest

import javax.xml.bind.ValidationException


object Validator {
  
  trait Validates[T] {
    def validate(i : SimpleItem[T], x: T): T  = {
        if(i.acceptedValues.isEmpty || i.acceptedValues.contains(x))
          x
        else
          throw new ValidationException(s"Invalid value: $x; Accepted values are: ${i.acceptedValues}")
      }
  }
  
  object Validates {
    implicit object ValidatesInt extends Validates[Int] 
    implicit object ValidatesDouble extends Validates[Double] 
    implicit object ValidatesBoolean extends Validates[Boolean]
    implicit object ValidatesString extends Validates[String]
  }
  
}