package com.github.rcmurphy.dietmodel

import com.github.rcmurphy.dietmodel.Database.getFood
import com.github.rcmurphy.scalamath.expression.{IntegerExpression, BigDecimalExpression, RuleSetExpression, ArrayExpression}
import com.github.rcmurphy.scalamath.mathematica._
import com.github.rcmurphy.scalamath._
import java.io.FileWriter
import org.slf4j.LoggerFactory
import scala.sys.process._
import scala.util.Try
import scala.xml.{NodeSeq, XML}

object Model {
  def logger = LoggerFactory.getLogger(getClass)

  def getVersion: String = {
    val nullProcessLogger = ProcessLogger(line => {}, line => {})
    Try("git describe --long --always".!!(nullProcessLogger).trim + "-" +
      "git rev-parse --abbrev-ref HEAD".!!(nullProcessLogger).trim).getOrElse("unknown")
  }
}

trait Model {
  implicit val mathematica = new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")

  def name: String

  def foods: List[Food]
  val enabledFoods = foods.filter(_.enabled)

  def nutrients: List[Nutrient]

  def proteinDiscount: BigDecimal

  def run(): String
  
  def calculateCostAndNutrients(): scala.Unit = {
    enabledFoods.foreach { food =>
      s"${food.id}std".a <~ food.unit.subdivisionsToHundredGrams * s"${food.id}div".a
    }
    "cost".a <~ "Plus".f(enabledFoods.map(food => food.cost * s"${food.id}std".a): _*)
    "adjustedCost".a <~ "cost".a - proteinDiscount * "protein".a
    nutrients.foreach { case Nutrient(nutrient, _, _, _, _) =>
      nutrient.a <~ "Plus".f(enabledFoods.map(food => food.nutrients(nutrient) * s"${food.id}std".a): _*)
    }
  }
  def generateOutput(dietDivs: RuleSetExpression, addlHeaders: Seq[String] = Seq()): String = {
    val foodDivRegex = """^([a-zA-Z0-9]+)div$""".r
    val optimalDiet = dietDivs.flatMap { case (foodDivRegex(foodId), amount) =>
      val value = amount match {
        case a: BigDecimalExpression => a.value
        case a: IntegerExpression => BigDecimal(a.value)
      }
      foods.find(_.id == foodId).map {
        food => (food, value * food.unit.subdivisionsToUnit)
      }
    }.toMap

    val cost = (mathematica ! "ReplaceAll".f("cost".a, dietDivs)).get
    val adjustedCost = (mathematica ! "ReplaceAll".f("adjustedCost".a, dietDivs)).get

    val protein = (mathematica ! "ReplaceAll".f("protein".a, dietDivs)).get

    val optimalNutrients =
      (mathematica ! "ReplaceAll".f(nutrients.map{n => n.id.a}, dietDivs)).get.asInstanceOf[ArrayExpression]
    val nutrientsWithValues = nutrients zip optimalNutrients.map{ case n: BigDecimalExpression => n.value.toFloat }
    val newLine = sys.props("line.separator")
    val resultBuilder = new StringBuilder()
    resultBuilder ++= "*** Model Parameters ***" + newLine
    resultBuilder ++= s"Name: $name" + newLine
    resultBuilder ++= s"Version: ${Model.getVersion}" + newLine
    resultBuilder ++= s"Protein Discount: ${proteinDiscount.formatted("%9.9f")}" + newLine
    addlHeaders.foreach(resultBuilder ++= _ + newLine)
    resultBuilder ++= "Prices:" + newLine
    foods.grouped(3).foreach { foodLine =>
      val priceStrs = foodLine.map(food => s"${(food.id + ":").padTo(15,' ')} ${food.cost.formatted("%5.5f")}")
      resultBuilder ++= priceStrs.mkString(" ") + newLine
    }
    resultBuilder ++= "Disabled Foods: " + foods.filterNot(_.enabled).map(_.id).mkString(" ") + newLine
    resultBuilder ++= "Unconstrained Nutrients: " + nutrients.filterNot(_.enforce).map(_.id).mkString(" ") + newLine
    resultBuilder ++= "*** Model Results ***" + newLine
    resultBuilder ++= s"Adjusted Cost: $adjustedCost" + newLine
    resultBuilder ++= s"Actual Cost: $cost" + newLine
    resultBuilder ++= s"${"Food".padTo(20, ' ')} ${"Per Day".padTo(16, ' ')} ${"Per Year".padTo(16, ' ')}" + newLine
    optimalDiet.toSeq.sortBy(_._1.id).foreach { case (Food(foodId, _, _, unit, cookingCoef, _), amount) =>
      val perYearAmount = (amount * unit.unitToHundredGrams * Unit.byName("Pound").hundredGramsToUnit * 365.25).toFloat
      resultBuilder ++= s"${foodId.padTo(20, ' ')} ${amount.toFloat.toString.padTo(11, ' ')} ${unit.name.padTo(4, ' ')} " +
        s"${perYearAmount.toString.padTo(11, ' ')} ${Unit.byName("Pound").name.padTo(4, ' ')}" + newLine
    }

    resultBuilder ++= "Nutrients:" + newLine
    nutrientsWithValues.sortBy(_._1.id).foreach { case (nutrient, value) =>
      val (maxClose, minClose) = (
        nutrient.maximum.map( max => (max, (max - value) < max * 0.05) ),
        nutrient.minimum.map( min => (min, (value - min) < min * 0.05 )))
      val unitInd = nutrient.unit.padTo(4, ' ')
      val (boundInd, bound) = ((maxClose), (minClose)) match {
        case (Some((max, true)), _) => ("⤒", s"Max: ${max.formatted("%8.2f")} $unitInd")
        case (_, Some((min, true))) => ("⤓", s"Min: ${min.formatted("%8.2f")} $unitInd")
        case _ => (" ", "")
      }
      val enforcedInd = if(!nutrient.enforce) "U" else " "
      resultBuilder ++= s"\t${nutrient.id.padTo(15, ' ')} $enforcedInd\t${value.formatted("%10.2f")} $unitInd\t$boundInd $bound" + newLine
    }
    resultBuilder.toString
  }

}
class OptimalModel(override val name: String, override val foods: List[Food], override val nutrients: List[Nutrient], override val proteinDiscount: BigDecimal, quantized: Boolean) extends Model {
  def logger = LoggerFactory.getLogger(getClass)

  def run(): String = {

    calculateCostAndNutrients()
    "foodConstraints".a <~ "And".f(enabledFoods.map(food => s"${food.id}div".a >= 0): _*)
    "quantizationConstraints".a <~ "Element[ " + enabledFoods.map(food => s"${food.id}div").mkString(" | ") +
      s" , ${if(quantized) "Integers" else "Reals"}]"
    "nutrientConstraints".a <~ nutrients.filter(_.enforce).flatMap {
      case Nutrient(nutrient, _, min, max, _) =>
        min.map( min => s"$nutrient >= $min") ++ max.map( max => s"$nutrient <= $max")
    }.mkString(" && ")
    val result: ArrayExpression =
      (mathematica ! s"Minimize[{adjustedCost,foodConstraints && nutrientConstraints && quantizationConstraints}, " +
        s"{ ${enabledFoods.map(food => s"${food.id}div").mkString(", ")} }]").get.asInstanceOf[ArrayExpression]

    val optimalDietDivs: RuleSetExpression = result(1).asInstanceOf[RuleSetExpression]

    val output = generateOutput(optimalDietDivs, Seq(s"Quantized: ${if(quantized) "Y" else "N"}"))

    logger.info("Closing Link")
    mathematica.close()
    logger.info("Link Closed")
    output
  }
}

class KnownDietModel(override val name: String, override val foods: List[Food], override val nutrients: List[Nutrient], val diet: KnownDiet, override val proteinDiscount: BigDecimal) extends Model {
  def logger = LoggerFactory.getLogger(getClass)

  val poundsPerYearToHundredGramsPerDay =
    Unit.byName("Pound").unitToHundredGrams / 365.25
  def run(): String = {
    calculateCostAndNutrients()
    val foodsNotPresentBuilder = StringBuilder.newBuilder
    val dietDivs: RuleSetExpression = foods.map {
      food =>
        (food.id + "div", diet.amounts.getOrElse(food.id, {
          foodsNotPresentBuilder ++= food.id + " "
          0.0
        }) * poundsPerYearToHundredGramsPerDay / food.unit.subdivisionsToHundredGrams)
    }.map{ case (k,v) => (k, BigDecimalExpression(v)) }.toMap
    val output = generateOutput(dietDivs,
      Seq(s"Diet Name: ${diet.name}",
          s"Foods Not Present: " + foodsNotPresentBuilder.toString,
        "########## Historical Diet ##########"))
    logger.info("Closing Link")
    mathematica.close()
    logger.info("Link Closed")
    output
  }
}

