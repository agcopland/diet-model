package com.github.rcmurphy.dietmodel.mathematica

import com.wolfram.jlink.{KernelLink, MathLinkFactory}
import org.slf4j.LoggerFactory

import scala.util.Try

class Mathematica(linkArgs: String) {
  class Variable(name: String) {
    protected val mathematica: Mathematica = Mathematica.this

    def <~(input: String): Try[_] = mathematica ! (name + " = " + input)
    def <~(input: Expression): Try[_] = mathematica ! (name + " = " + input.toMathematica)

    def get: Try[Expression] = mathematica ! name
  }
  def logger = LoggerFactory.getLogger(getClass)
  val link: KernelLink = MathLinkFactory.createKernelLink(linkArgs)

  link.discardAnswer()

  def !(input: Expression): Try[Expression] =  this.!(input.toMathematica)

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

