package com.github.mdelem.scalaquest

object Validator {
  
  trait Validates[T] {
    def validate(x: Any): T
  }
  
  object Validates {
    implicit object ValidatesInt extends Validates[Int] {
      def validate(x: Any): Int = x.asInstanceOf[Int]
    }
    
    implicit object ValidatesBoolean extends Validates[Boolean] {
      def validate(x: Any): Boolean = x.asInstanceOf[Boolean]
    }
    
  }
  
}