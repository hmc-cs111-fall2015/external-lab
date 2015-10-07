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

case class Num(n: Int) extends Expr
case class Plus(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Times(left: Expr, right: Expr) extends Expr
case class Neg(left: Expr) extends Expr