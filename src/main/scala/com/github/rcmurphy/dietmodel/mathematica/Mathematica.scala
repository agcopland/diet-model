package com.github.rcmurphy.dietmodel.mathematica

import com.wolfram.jlink._
import org.slf4j.LoggerFactory

import scala.util.Try

class Mathematica(linkArgs: String) {
  class Variable(name: String) {
    protected val mathematica: Mathematica = Mathematica.this

    def <~(input: String): Try[_] = mathematica ! (name + " = " + input)
    def <~(input: Expression): Try[_] = mathematica ! FunctionExpression("Set", Seq(name.a, input))

    def get: Try[Expression] = mathematica ! name.a
  }
  def logger = LoggerFactory.getLogger(getClass)
  val link: KernelLink = MathLinkFactory.createKernelLink(linkArgs)

  // Log Error Messages
  link.addPacketListener(new PacketListener {
    override def packetArrived(evt: PacketArrivedEvent): Boolean = {
      if (evt.getPktType() == MathLink.TEXTPKT || evt.getPktType() == MathLink.MESSAGEPKT) {
        val ml: KernelLink = evt.getSource().asInstanceOf[KernelLink]
        logger.warn(ml.getString)
      }
      false
    }
  })

  link.discardAnswer()

  def !(input: Expression): Try[Expression] = {
    logger.debug(s"Sending to Kernel: $input")
    val result = Try(link.synchronized {
      sendToMathematica(FunctionExpression.apply("EvaluatePacket", Seq(input)))
      link.endPacket()
      link.waitForAnswer()
      link.getExpr
    })
    result.map(result => logger.trace(s"Received from Kernel (Raw): $result"))
    val parsedResult = result.map(Expression(_))
    parsedResult.map(result => logger.debug(s"Received from Kernel (Parsed): $result"))
    parsedResult
  }

  private def sendToMathematica(expression: Expression): Unit = {
    expression match {
      case expr: ArrayExpression =>
        logger.trace(s"Sending List (count: '${expr.values.size}')")
        link.putFunction("List", expr.values.size)
        expr.values.foreach(sendToMathematica)
      case expr: AtomExpression =>
        logger.trace(s"Sending Atom (name: '${expr.name}')")
        link.putSymbol(expr.name)
      case expr: BigDecimalExpression =>
        logger.trace(s"Sending Big Decimal (value: '${expr.value}')")
        link.put(expr.value)
      case expr: IntegerExpression =>
        logger.trace(s"Sending Integer (value: '${expr.value}')")
        // Awkward, but needed to disambiguate the method being called
        link.put(expr.value.asInstanceOf[AnyRef])
      case expr: FunctionExpression =>
        logger.trace(s"Sending Function (name: '${expr.name}', arg count: '${expr.values.size}')")
        link.putFunction(expr.name, expr.values.size)
        expr.values.foreach(sendToMathematica)
      case expr: RuleSetExpression =>
        sendToMathematica(ruleListToListRule(expr))
    }
  }

  def !(input: String): Try[Expression] = {
    logger.debug(s"Sending to Kernel: $input")
    val result = Try(link.synchronized {
      link.evaluate(input)
      link.waitForAnswer()
      link.getExpr
    })
    result.map(result => logger.trace(s"Received from Kernel (Raw): $result"))
    val parsedResult = result.map(Expression(_))
    parsedResult.map(result => logger.debug(s"Received from Kernel (Parsed): $result"))
    parsedResult
  }

  def apply(variableName: String): Variable = {
    new Variable(variableName)
  }

  def close() = {
    link.close()
  }

}

