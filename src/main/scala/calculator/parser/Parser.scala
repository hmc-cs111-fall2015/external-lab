package calculator.parser

import scala.util.parsing.combinator._
import calculator.ir._
import calculator.ir.sugar._

object CalcParser extends JavaTokenParsers with PackratParsers {

    // parsing interface
    def apply(s: String): ParseResult[AST] = parseAll(expr, s)

    // expressions
    lazy val expr: PackratParser[Expr] = 
      (   expr~"+"~fact ^^ {case l~"+"~r ⇒ l |+| r}
        | expr~"-"~fact ^^ {case l~"-"~r => l |-| r}
        | term )
        
    // terms
    lazy val term: PackratParser[Expr] =
      (   term~"*"~fact ^^ {case l~"*"~r ⇒ l |*| r}
        | term~"/"~fact ^^ {case l~"/"~r ⇒ l |/| r}
        | fact )
        
    // factors
    lazy val fact: PackratParser[Expr] =
      number
      
    // numbers
    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}
    
 }
