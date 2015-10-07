package calculator

import calculator.ir._

package object semantics {
  def eval(ast: AST): Int = ast match {
    case Num(i) ⇒ i
    case Plus(left, right) ⇒ eval(left) + eval(right)
    case Sub(left, right) => eval(left) - eval(right)
    case Times(left, right) ⇒ eval(left) * eval(right)
    case Div(left, right) => eval(left) / eval(right)
  }
}