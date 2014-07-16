package com.github.rcmurphy.dietmodel

package object mathematica {
  implicit def bigDoubleToBigDoubleExpr(value: Double) = BigDecimalExpression(value)
  implicit def intToIntExpr(value: Int) = IntegerExpression(value)
  implicit def intExprToInt(expr: IntegerExpression) = expr.value
  implicit def seqToArrayExpr(seq: Seq[Expression]) = ArrayExpression(seq)
}
