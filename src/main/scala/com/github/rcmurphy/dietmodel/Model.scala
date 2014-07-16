package com.github.rcmurphy.dietmodel

import com.github.rcmurphy.dietmodel.mathematica._
import org.slf4j.LoggerFactory
import com.github.rcmurphy.dietmodel.Unit._
import com.github.rcmurphy.dietmodel.Database.getFood
object Model {
  def logger = LoggerFactory.getLogger(getClass)
  val mathematica = new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")

  val nutrients = List(
    Nutrient("calcium", "mg", minimum = Some(500), maximum = None),
    Nutrient("carbohydrates", "g", minimum = Some(300), maximum = Some(950)),
    Nutrient("choline", "mg", minimum = Some(450), maximum = None),
    Nutrient("cholesterol", "mg", minimum = Some(135), maximum = Some(450)),
    Nutrient("energy", "kcal", minimum = Some(3900), maximum = Some(4500)),
    Nutrient("fiber", "g", minimum = Some(35), maximum = Some(180)),
    Nutrient("iron", "mg", minimum = Some(10), maximum = Some(80)),
    Nutrient("potassium", "mg", minimum = Some(4000), maximum = Some(8000)),
    Nutrient("phosphorus", "mg", minimum = Some(480), maximum = Some(4000)),
    Nutrient("protein", "g", minimum = Some(50), maximum = None),
    Nutrient("riboflavin", "mg", minimum = Some(1.1), maximum = None),
    Nutrient("saturatedfat", "g", minimum = Some(20), maximum = Some(55)),
    Nutrient("sodium", "mg", minimum = Some(2000), maximum = Some(5000)),
    Nutrient("sugar", "g", minimum = Some(12), maximum = Some(80)),
    Nutrient("totalfat", "g", minimum = Some(65), maximum = Some(130)),
    Nutrient("vitamina", "IU", minimum = Some(5000), maximum = None),
    Nutrient("vitaminb12", "μg", minimum = Some(2.0), maximum = None),
    Nutrient("vitaminc", "mg", minimum = Some(40), maximum = None),
    Nutrient("zinc", "mg", minimum = Some(9.4), maximum = Some(40))
  )

  val foods = List(
    getFood("Beef, carcass, separable lean and fat, select, raw", 0.01881, QuarterPound),
    getFood("Butter, without salt", 0.0275, EighthPound),
    getFood("Corn, white, steamed (Navajo)", 0.00183, BushelCorn),
    getFood("Wheat flour, whole-grain", 0.0037, SixteenthDryLitre, id = Some("flour")),
    getFood("Milk, whole, 3.25% milkfat, without added vitamin A and vitamin D", 0.003496, QuarterPound),
    getFood("Mutton, cooked, roasted (Navajo)", 0.025, EighthPound),
    getFood("Oats", 0.005, BushelOats),
    getFood("Peas, split, mature seeds, raw", 0.0041, BushelPeas),
    getFood("Pork, cured, bacon, raw", 0.02475, QuarterPound),
    getFood("Sweet potato, raw, unprepared", 0.0026, BushelPotatoes, id = Some("sweetpotatoes")),
    getFood("Potatoes, white, flesh and skin, raw", 0.00352, BushelPotatoes, id = Some("whitepotatoes"))
  ).map(_.get)

  def main (args: Array[String]) {

    foods.foreach { food =>
      mathematica(s"${food.id}std") <~ food.unit.subdivisionsToHundredGrams * AtomExpression(s"${food.id}div")
    }
    mathematica("cost") <~ foods.map(food => s"${food.cost} * ${food.id}std").mkString(" + ")
    mathematica("adjustedCost") <~ "cost - 0.0002389 * protein"
    nutrients.foreach { case Nutrient(nutrient, _,  _, _) =>
      mathematica(nutrient) <~ foods.map(food => s"${food.nutrients(nutrient)} * ${food.id}std").mkString(" + ")
    }
    mathematica("foodConstraints") <~ foods.map(food => s"${food.id}div >= 0").mkString(" && ")
    mathematica("quantizationConstraints") <~ "Element[ " + foods.map(food => s"${food.id}div").mkString(" | ") + " , Integers]"
    mathematica("nutrientConstraints") <~ nutrients.flatMap {
      case Nutrient(nutrient, _, min, max) =>
        min.map( min => s"$nutrient >= ${min}") ++ max.map( max => s"$nutrient <= ${max}")
    }.mkString(" && ")
    val result: ArrayExpression =
      (mathematica ! s"Minimize[{adjustedCost,foodConstraints && nutrientConstraints && quantizationConstraints}, " +
        s"{ ${foods.map(food => s"${food.id}div").mkString(", ")} }]").get.asInstanceOf[ArrayExpression]

    val optimalDietDivs: RuleListExpression = result(1).asInstanceOf[RuleListExpression]


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

    val cost = (mathematica ! FunctionExpression("ReplaceAll", Seq(AtomExpression("cost"), optimalDietDivs))).get

    val protein = (mathematica ! FunctionExpression("ReplaceAll", Seq(AtomExpression("protein"), optimalDietDivs))).get

    val optimalNutrients = (mathematica ! FunctionExpression("ReplaceAll",
      Seq((nutrients.map{n => AtomExpression(n.id)}), optimalDietDivs))).get.asInstanceOf[ArrayExpression]
    val nutrientsWithValues = nutrients zip optimalNutrients.map{ case n: BigDecimalExpression => n.value.toFloat }

    logger.info(s"Adjusted Cost: ${result(0)}")
    logger.info(s"Actual Cost: $cost")
    logger.info(s"${"Food".padTo(20, ' ')} ${"Per Day".padTo(16, ' ')} ${"Per Year".padTo(16, ' ')}")
    optimalDiet.foreach { case (Food(foodId, _, _, unit), amount) =>
      val perYearAmount = (amount * unit.unitToHundredGrams * Pound.hundredGramsToUnit * 365.25).toFloat
      logger.info(s"${foodId.padTo(20, ' ')} ${amount.toFloat.toString.padTo(11, ' ')} ${unit.name.padTo(4, ' ')} " +
      s"${perYearAmount.toString.padTo(11, ' ')} ${Pound.name.padTo(4, ' ')}")
    }

    logger.info("Nutrients:")
    nutrientsWithValues.foreach { case (nutrient, value) =>
      val (maxClose, minClose) = (
        nutrient.maximum.map( max => (max - value) < max * 0.05 ),
        nutrient.minimum.map( min => (value - min) < min * 0.05 ))
      val indicator = (maxClose, minClose) match {
        case (Some(true), _) => "⤒"
        case (_, Some(true)) => "⤓"
        case _ => ""
      }
      logger.info(s"${nutrient.id.padTo(20, ' ')} ${value} ${nutrient.unit} $indicator")
    }
    mathematica.close()
  }

}

