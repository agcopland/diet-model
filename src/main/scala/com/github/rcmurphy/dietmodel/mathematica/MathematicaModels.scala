package com.github.rcmurphy.dietmodel.mathematica

import com.wolfram.jlink.{Expr => JLinkExpr}
import org.slf4j.LoggerFactory

sealed trait Expression {

  def *(other: Expression): Expression = {
    FunctionExpression("Times", Seq(this, other))
  }

  def +(other: Expression): Expression = {
    FunctionExpression("Plus", Seq(this, other))
  }

  def toMathematica: String
}
sealed trait ConstantExpression[T] extends Expression {
  def value: T
  override def toString: String = value.toString
  override def toMathematica: String = value.toString
}

object Expression {
  def logger = LoggerFactory.getLogger(getClass)
  def apply(expr: JLinkExpr): Expression = {
    if(expr.listQ())
    {
      // Assume we have an array
      val children = expr.args().map(apply)
      if(children.forall {
        case child: FunctionExpression if child.name == "Rule" && child.values.length == 2 => true
        case _ => false
      }) {
        RuleListExpression(children.map {
          case FunctionExpression("Rule", Seq(name, value)) => (name.toString, value)
        }.toMap)
      } else {
        ArrayExpression(children)
      }
    } else if(expr.integerQ()) {
      IntegerExpression(expr.asInt())
    } else if(expr.numberQ() && !expr.complexQ()) {
      BigDecimalExpression(expr.asBigDecimal())
    } else if(expr.atomQ()) {
      AtomExpression(expr.asString())
    } else {
      FunctionExpression(expr.head().asString(), expr.args().map(apply))
    }
  }

  implicit def integerResultToInteger(result: IntegerExpression): Int =
  {
    result.value
  }
}
case class ArrayExpression(values: Seq[Expression])
  extends Expression with Seq[Expression] {
  override def length: Int = values.length

  override def iterator: Iterator[Expression] = values.iterator

  override def apply(idx: Int): Expression = values.apply(idx)

  override def toString(): String = values.mkString("{ ", " , "," }")

  override def toMathematica: String = values.map(_.toMathematica).mkString("{ ", " , "," }")
}
case class RuleListExpression(rules: Map[String, Expression])
  extends Expression with Map[String, Expression] {
  override def +[B1 >: Expression](kv: (String, B1)): Map[String, B1] = rules + kv

  override def get(key: String): Option[Expression] = rules.get(key)

  override def iterator: Iterator[(String, Expression)] = rules.iterator

  override def -(key: String): Map[String, Expression] = rules - key

  override def toString(): String = rules.map { case (name, value) => s"$name ← $value" }.mkString("ℜ{ ", " , "," }")

  override def toMathematica: String =
    rules.map { case (name, value) => s"Rule[$name, ${value.toMathematica}]" }.mkString("{ ", " , "," }")
}

case class AtomExpression(name: String) extends Expression {
  override def toString: String = name

  override def toMathematica: String = name
}

case class IntegerExpression(value: Int) extends ConstantExpression[Int]

case class BigDecimalExpression(value: BigDecimal) extends ConstantExpression[BigDecimal] {
  override def toString: String = BigDecimalExpression.formatter.format(value)
}

object BigDecimalExpression {
  protected val formatter = new java.text.DecimalFormat("0.00######")
}

case class FunctionExpression(name: String, values: Seq[Expression]) extends Expression {
  import FunctionExpression._

  override def toString: String = (name, values) match {
    case (_, Seq(a, b)) if binaryOperators.contains(name) =>
      val as = autoWrap(a)
      val bs = autoWrap(b)
      as + " " + binaryOperators(name) + " " + bs
    case (_, values) if naryOperators.contains(name) =>
      val valueStrings = values.map(autoWrap)
      valueStrings.mkString(" " + naryOperators(name) + " ")
    case _ => name + values.mkString("( ", " , ", " )")
  }

  private def autoWrap(exp: Expression): String = {
    exp match {
      case v: FunctionExpression if operators.contains(v.name) && operators.contains(this.name) => s"( $v )"
      case v => v.toString
    }
  }

  override def toMathematica: String = s"$name" + values.map(_.toMathematica).mkString("[ ", " , "," ]")
}
object FunctionExpression {
  val binaryOperators = Map(
    /* Equality */
    "LessEqual" -> "≤",
    "GreaterEqual" -> "≥",
    "Rule" -> "←",
    /* Misc */
    "Element" -> "∈"
  )
  val naryOperators = Map(
    /* Arithmetic */
    "Plus" -> "+",
    "Times" -> "⨉",
    /* Boolean */
    "And" -> "&&",
    /* Misc */
    "Alternatives" -> "|"
  )
  val operators = binaryOperators ++ naryOperators
}