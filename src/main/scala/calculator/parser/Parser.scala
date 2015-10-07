package calculator.parser

import scala.util.parsing.combinator._
import calculator.ir._
import calculator.ir.sugar._

object CalcParser extends JavaTokenParsers with PackratParsers {

    // parsing interface
    def apply(s: String): ParseResult[AST] = parseAll(expr, s)

    // expressions
    lazy val expr: PackratParser[Expr] = 
      (   "-" ~ expr ^^ {case "-"~e => new Neg(e) }
        |  expr~"+"~fact ^^ {case l~"+"~r => l |+| r}
        |  expr~"-"~fact ^^ {case l~"-"~r => l |-| r}
        | "(" ~ expr ~ ")" ^^ {case "("~e~")" => e}
        | fact )
        
    lazy val fact: PackratParser[Expr] =
      ( fact~"*"~term ^^ {case l~"*"~r => l |*| r}
        | term )
      
    // factors
    lazy val term: PackratParser[Expr] =
      number
      
    // numbers
    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}
    
 }
