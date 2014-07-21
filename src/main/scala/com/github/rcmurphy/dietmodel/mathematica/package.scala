package com.github.rcmurphy.dietmodel

package object mathematica {
  implicit def bigDoubleToBigDoubleExpr(value: Double) = BigDecimalExpression(value)
  implicit def intToIntExpr(value: Int) = IntegerExpression(value)
  implicit def intExprToInt(expr: IntegerExpression) = expr.value
  implicit def seqToArrayExpr(seq: Seq[Expression]) = ArrayExpression(seq)

  /* Atom Expression Builder (allows "x".a -> AtomExpression("x")) */

  implicit def strToAtomBuilder(str: String) = AtomExpressionBuilder(str)

  implicit def ruleListToListRule(ruleList: RuleSetExpression) = {
    ArrayExpression(ruleList.rules.map { case (name, value) => FunctionExpression("Rule", Seq(name.a, value)) }.toSeq)
  }
}
