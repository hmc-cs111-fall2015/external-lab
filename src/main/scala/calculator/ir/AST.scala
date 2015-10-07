package calculator.ir

/**
 * -----------
 * Grammar
 * -----------
 * 
 *                   n ∈ 𝒵 
 * 
 *       e ∈ Expr ::= e + t | e - t | t
 *       t ∈ Term ::= t * f | t / f | f
 *       f ∈ Fact ::= n | ( e )
 *  
 */

sealed abstract class AST
sealed abstract class Expr extends AST
sealed abstract class Factor extends Expr
sealed abstract class Term extends Expr

case class Plus(left: Expr, right: Term) extends Expr
case class Minus(left: Expr, right: Term) extends Expr
case class Times(left: Term, right: Factor) extends Term
case class Quotient(left: Term, right: Factor) extends Term

case class Num(n: Int) extends Factor
case class Parenthetical(inner: Expr) extends Factor
