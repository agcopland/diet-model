package com.github.rcmurphy.dietmodel

import com.github.rcmurphy.dietmodel.Model._
import com.github.rcmurphy.dietmodel.Unit._
import com.github.rcmurphy.dietmodel.Database.getFood
import com.github.rcmurphy.scalamath.mathematica._
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
        proteinDiscount <- (nutrientXml \ "@proteinDiscount").textOpt.map(BigDecimal(_))
        quantized <- (nutrientXml \ "@quantized").textOpt.map(_.toBoolean)
      } yield {
        ModelParams(name, foodSet, nutrientSet, proteinDiscount, quantized)
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

  def getFoods(setName: String) = {
    (XML.loadFile(s"./data/foodsets/$setName.xml") \ "food").flatMap { foodXml =>
      val name = (foodXml \ "@name").text
      val price = (foodXml \ "@price").text.toDouble
      val unit = Unit.byName((foodXml \ "@unit").text)
      val id = (foodXml \ "@id").textOpt
      val cookingCoef = (foodXml \ "@cookingCoef").textOpt.map(_.toDouble).getOrElse(1.0)
      val enabled = (foodXml \ "@enabled").textOpt.map(_.toBoolean).getOrElse(true)
      getFood(name, price, unit, id, cookingCoef, enabled)
    }.toList
  }

  def main(args: Array[String]) {
    getModelParams.foreach{ modelParams =>
      new Model(
        modelParams.name,
        getFoods(modelParams.foodSet),
        getNutrients(modelParams.nutrientSet),
        modelParams.proteinDiscount,
        modelParams.quantized
      ).run()
    }
  }
}
class Model(name: String, foods: List[Food], nutrients: List[Nutrient], proteinDiscount: BigDecimal, quantized: Boolean) {
  def logger = LoggerFactory.getLogger(getClass)
  val mathematica = new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")

  def run() = {
    val enabledFoods = foods.filter(_.enabled)
    enabledFoods.foreach { food =>
      mathematica(s"${food.id}std") <~ food.unit.subdivisionsToHundredGrams * AtomExpression(s"${food.id}div")
    }
    mathematica("cost") <~ enabledFoods.map(food => s"${food.cost} * ${food.id}std").mkString(" + ")
    mathematica("adjustedCost") <~ "cost".a - proteinDiscount * "protein".a
    nutrients.foreach { case Nutrient(nutrient, _,  _, _, _) =>
      mathematica(nutrient) <~ enabledFoods.map(food => s"${food.nutrients(nutrient)} * ${food.id}std").mkString(" + ")
    }
    mathematica("foodConstraints") <~ enabledFoods.map(food => s"${food.id}div >= 0").mkString(" && ")
    mathematica("quantizationConstraints") <~ "Element[ " + enabledFoods.map(food => s"${food.id}div").mkString(" | ") +
      s" , ${if(quantized) "Integers" else "Reals"}]"
    mathematica("nutrientConstraints") <~ nutrients.filter(_.enforce).flatMap {
      case Nutrient(nutrient, _, min, max, _) =>
        min.map( min => s"$nutrient >= ${min}") ++ max.map( max => s"$nutrient <= ${max}")
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

    val cost = (mathematica ! FunctionExpression("ReplaceAll", Seq("cost".a, optimalDietDivs))).get

    val protein = (mathematica ! FunctionExpression("ReplaceAll", Seq("protein".a, optimalDietDivs))).get

    val optimalNutrients = (mathematica ! FunctionExpression("ReplaceAll",
      Seq((nutrients.map{n => n.id.a}), optimalDietDivs))).get.asInstanceOf[ArrayExpression]
    val nutrientsWithValues = nutrients zip optimalNutrients.map{ case n: BigDecimalExpression => n.value.toFloat }
    logger.info("*** Model Parameters ***")
    logger.info(s"Name: $name")
    logger.info(s"Version: $getVersion")
    logger.info(s"Quantized: ${if(quantized) "Y" else "N"}")
    logger.info(s"Protein Discount: ${proteinDiscount.formatted("%9.9f")}")
    logger.info("Prices:")
    foods.grouped(3).foreach { foodLine =>
      val priceStrs = foodLine.map(food => s"${(food.id + ":").padTo(15,' ')} ${food.cost.formatted("%5.5f")}")
      logger.info(priceStrs.mkString(" "))
    }
    logger.info("Disabled Foods: " + foods.filterNot(_.enabled).map(_.id).mkString(" "))
    logger.info("Unconstrained Nutrients: " + nutrients.filterNot(_.enforce).map(_.id).mkString(" "))
    logger.info("*** Model Results ***")
    logger.info(s"Adjusted Cost: ${result(0)}")
    logger.info(s"Actual Cost: $cost")
    logger.info(s"${"Food".padTo(20, ' ')} ${"Per Day".padTo(16, ' ')} ${"Per Year".padTo(16, ' ')}")
    optimalDiet.toSeq.sortBy(_._1.id).foreach { case (Food(foodId, _, _, unit, cookingCoef, _), amount) =>
      val perYearAmount = (amount * unit.unitToHundredGrams * Unit.byName("Pound").hundredGramsToUnit * 365.25).toFloat
      logger.info(s"${foodId.padTo(20, ' ')} ${amount.toFloat.toString.padTo(11, ' ')} ${unit.name.padTo(4, ' ')} " +
      s"${perYearAmount.toString.padTo(11, ' ')} ${Unit.byName("Pound").name.padTo(4, ' ')}")
    }

    logger.info("Nutrients:")
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
      logger.info(s"\t${nutrient.id.padTo(15, ' ')} $enforcedInd\t${value.formatted("%10.2f")} $unitInd\t$boundInd $bound")
    }
    logger.info("Closing Link")
    mathematica.close()
    logger.info("Link Closed")
  }

}

