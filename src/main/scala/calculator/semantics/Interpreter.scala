package calculator

import calculator.ir._

package object semantics {
  def eval(ast: AST): Int = ast match {
    case Num(i) ⇒ i
    case Plus(left, right) ⇒ eval(left) + eval(right)
    case Minus(left, right) => eval(left) - eval(right)
    case Multiply(left, right) => eval(left) * eval(right)
    case Divide(left, right) => eval(left) / eval(right)
  }
}