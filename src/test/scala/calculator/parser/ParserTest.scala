package language.parser

import scala.language.implicitConversions
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import calculator.parser.CalcParser
import calculator.ir._
import calculator.ir.sugar._

object CalcParseSpec extends Properties("Parser") {

    // some syntactic sugar for expressing parser tests
    implicit class ParseResultChecker(input: String) {
      def ~>(output: Expr) = {
        val result = CalcParser(input)
        result.successful && result.get == output
      }
    }
    
    property("numbers") = forAll { (n: Int) ⇒
      s"$n" ~> n
    } 
    
    property("addition") = forAll { (n1: Int, n2: Int) ⇒
      s"$n1 + $n2" ~> (n1 |+| n2)   
    } 
    
        
    property("subtraction") = forAll { (n1: Int, n2: Int) ⇒
      s"$n1 - $n2" ~> (n1 |-| n2)  
    } 
    
    property("multiplication") = forAll { (n1: Int, n2: Int) ⇒
      s"$n1 * $n2" ~> (n1 |*| n2)    
    } 
    
    property("division") = forAll { (n1: Int, n2: Int) ⇒
      s"$n1 / $n2" ~>  (n1 |/| n2)    
    } 
}