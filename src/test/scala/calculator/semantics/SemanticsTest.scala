package language.interpreter

import scala.language.implicitConversions
import org.scalacheck._
import org.scalacheck.Prop.{forAll,BooleanOperators,throws}
import Gen._
import Arbitrary.arbitrary
import calculator.ir._
import calculator.ir.sugar._
import calculator.semantics.eval

object CalcInterpreterSpec extends Properties("Interpreter") {

    // some syntactic sugar for expressing interpreter tests
    implicit class TreeChecker(input: Expr) {
      def ~>(output: Int) = {
        val result = eval(input)
        result == output
      }
      
      def ~/~>[T <: Throwable](c: Class[T]) = {
        throws(c){eval(input)}
      }
    }
    
    // Generators for ASTs
    val genNum = for {
      n ← arbitrary[Int]
    } yield Num(n)
    
    val genAddition = for {
      n1 ← genNum
      n2 ← genNum
    } yield n1 |+| n2
    
    property("numbers") = forAll { (n: Int) ⇒
      Num(n) ~> n
    } 
    
    property("addition") = forAll { (n1: Int, n2: Int) ⇒
      (n1 |+| n2) ~> (n1 + n2)   
    } 
    
    property("subtraction") = forAll { (n1: Int, n2: Int) ⇒
      (n1 |-| n2) ~> (n1 - n2)   
    } 
    
}