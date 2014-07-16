package com.github.rcmurphy.dietmodel.mathematica

import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

class MathematicaSpec extends FlatSpec with Matchers {
  def logger = LoggerFactory.getLogger(getClass)

  val mathematica: Mathematica = {
    logger.debug("Connecting to kernel...")
    MathematicaRunner.mathematica
  }

  "The Mathematica Engine" must "start" in {
    val result: IntegerExpression = (mathematica ! "2+2").get.asInstanceOf[IntegerExpression]
    result.value should equal (4)
  }

  it must "accept integers and BigDecimals" in {
    (mathematica ! 4) == (4)
  }

}

object MathematicaRunner {
  lazy val mathematica: Mathematica =
    new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")
}
