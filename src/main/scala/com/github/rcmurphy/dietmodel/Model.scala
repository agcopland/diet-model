package com.github.rcmurphy.dietmodel

import com.github.rcmurphy.dietmodel.Model._
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

  private def getVersion: String = {
    val nullProcessLogger = ProcessLogger(line => {}, line => {})
    Try("git describe --long --always".!!(nullProcessLogger).trim + "-" +
      "git rev-parse --abbrev-ref HEAD".!!(nullProcessLogger).trim).getOrElse("unknown")
  }

  implicit def enrichNodeSeq(nodeSeq: NodeSeq) = new AnyRef {
    def textOpt : Option[String] = {
      val text = nodeSeq.text
      if (text == null || text.length == 0) None else Some(text)
    }
  }
  def strOptToOptDouble(input: String): Option[Double] = {
    if(input != "None") {
      Some(input.toDouble)
    } else {
      None
    }
  }
  def getModelParams = {
    (XML.loadFile(s"./data/models/models.xml") \ "model").flatMap { nutrientXml =>
      for {
        name <- (nutrientXml \ "@name").textOpt
        foodSet <- (nutrientXml \ "@foodSet").textOpt
        nutrientSet <- (nutrientXml \ "@nutrientSet").textOpt
        priceSet <- (nutrientXml \ "@priceSet").textOpt
        proteinDiscount <- (nutrientXml \ "@proteinDiscount").textOpt.map(BigDecimal(_))
        quantized <- (nutrientXml \ "@quantized").textOpt.map(_.toBoolean)
      } yield {
        ModelParams(name, foodSet, nutrientSet, priceSet, proteinDiscount, quantized)
      }
    }.toList
  }

  def getNutrients(setName: String) = {
    (XML.loadFile(s"./data/nutrientsets/$setName.xml") \ "nutrient").flatMap { nutrientXml =>
      for {
        name <- (nutrientXml \ "@name").textOpt
        unit <- (nutrientXml \ "@unit").textOpt
        minimum <- (nutrientXml \ "@minimum").textOpt.map(strOptToOptDouble)
        maximum <- (nutrientXml \ "@maximum").textOpt.map(strOptToOptDouble)
      } yield {
        val enforce = (nutrientXml \ "@enforce").textOpt.map(_.toBoolean).getOrElse(true)
        Nutrient(name, unit, minimum, maximum, enforce)
      }
    }.toList
  }

  def getPrices(setName: String) = {
    (XML.loadFile(s"./data/pricesets/$setName.xml") \ "price").flatMap { nutrientXml =>
      for {
        id <- (nutrientXml \ "@id").textOpt
        price <- (nutrientXml \ "@price").textOpt.map(_.toDouble)
      } yield {
        (id, price)
      }
    }.toMap
  }

  def getFoods(setName: String, priceSet: Map[String, Double]): List[Food] = {
    (XML.loadFile(s"./data/foodsets/$setName.xml") \ "food").flatMap { foodXml =>
      for {
        name <- (foodXml \ "@name").textOpt
        unit <- (foodXml \ "@unit").textOpt.map(Unit.byName(_))
      } yield {
        val id = (foodXml \ "@id").textOpt.getOrElse(name.split(" ")(0).toLowerCase.replace(",", ""))
        val cookingCoef = (foodXml \ "@cookingCoef").textOpt.map(_.toDouble).getOrElse(1.0)
        val enabled = (foodXml \ "@enabled").textOpt.map(_.toBoolean).getOrElse(true)
        val price = priceSet(id)
        getFood(id, name, price, unit, cookingCoef, enabled).get
      }
    }.toList
  }
  def using[A <: {def close(): scala.Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }
  def writeToFile(fileName:String, data:String) =
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def main(args: Array[String]) {
    getModelParams.foreach{ modelParams =>
      val priceSet = getPrices(modelParams.priceSet)
      val result = new Model(
        modelParams.name,
        getFoods(modelParams.foodSet, priceSet),
        getNutrients(modelParams.nutrientSet),
        modelParams.proteinDiscount,
        modelParams.quantized
      ).run()

      writeToFile(s"output/model-${modelParams.name}-result.txt", result)
    }
  }
}
class Model(name: String, foods: List[Food], nutrients: List[Nutrient], proteinDiscount: BigDecimal, quantized: Boolean) {
  def logger = LoggerFactory.getLogger(getClass)
  implicit val mathematica = new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")

  def run(): String = {
    val enabledFoods = foods.filter(_.enabled)
    enabledFoods.foreach { food =>
      s"${food.id}std".a <~ food.unit.subdivisionsToHundredGrams * s"${food.id}div".a
    }
    "cost".a <~ "Plus".f(enabledFoods.map(food => food.cost * s"${food.id}std".a): _*)
    "adjustedCost".a <~ "cost".a - proteinDiscount * "protein".a
    nutrients.foreach { case Nutrient(nutrient, _, _, _, _) =>
      nutrient.a <~ "Plus".f(enabledFoods.map(food => food.nutrients(nutrient) * s"${food.id}std".a): _*)
    }
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


    val foodDivRegex = """^([a-zA-Z0-9]+)div$""".r
    val optimalDiet = optimalDietDivs.flatMap { case (foodDivRegex(foodId), amount) =>
      val value = amount match {
        case a: BigDecimalExpression => a.value
        case a: IntegerExpression => BigDecimal(a.value)
      }
      foods.find(_.id == foodId).map {
        food => (food, value * food.unit.subdivisionsToUnit)
      }
    }.toMap

    val cost = (mathematica ! "ReplaceAll".f("cost".a, optimalDietDivs)).get

    val protein = (mathematica ! "ReplaceAll".f("protein".a, optimalDietDivs)).get

    val optimalNutrients =
      (mathematica ! "ReplaceAll".f(nutrients.map{n => n.id.a}, optimalDietDivs)).get.asInstanceOf[ArrayExpression]
    val nutrientsWithValues = nutrients zip optimalNutrients.map{ case n: BigDecimalExpression => n.value.toFloat }
    val newLine = sys.props("line.separator")
    val resultBuilder = new StringBuilder()
    resultBuilder ++= "*** Model Parameters ***" + newLine
    resultBuilder ++= s"Name: $name" + newLine
    resultBuilder ++= s"Version: $getVersion" + newLine
    resultBuilder ++= s"Quantized: ${if(quantized) "Y" else "N"}" + newLine
    resultBuilder ++= s"Protein Discount: ${proteinDiscount.formatted("%9.9f")}" + newLine
    resultBuilder ++= "Prices:" + newLine
    foods.grouped(3).foreach { foodLine =>
      val priceStrs = foodLine.map(food => s"${(food.id + ":").padTo(15,' ')} ${food.cost.formatted("%5.5f")}")
      resultBuilder ++= priceStrs.mkString(" ") + newLine
    }
    resultBuilder ++= "Disabled Foods: " + foods.filterNot(_.enabled).map(_.id).mkString(" ") + newLine
    resultBuilder ++= "Unconstrained Nutrients: " + nutrients.filterNot(_.enforce).map(_.id).mkString(" ") + newLine
    resultBuilder ++= "*** Model Results ***" + newLine
    resultBuilder ++= s"Adjusted Cost: ${result(0)}" + newLine
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
    logger.info("Closing Link")
    mathematica.close()
    logger.info("Link Closed")
    resultBuilder.toString
  }

}

