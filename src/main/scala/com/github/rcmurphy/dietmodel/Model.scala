package com.github.rcmurphy.dietmodel

import com.github.rcmurphy.dietmodel.mathematica._
import org.slf4j.LoggerFactory
import com.github.rcmurphy.dietmodel.Unit._
import com.github.rcmurphy.dietmodel.Database.getFood
object Model {
  def logger = LoggerFactory.getLogger(getClass)
  val mathematica = new Mathematica("-linkmode launch -linkname '\"/Applications/Mathematica.app/Contents/MacOS/MathKernel\" -mathlink'")

  val nutrients = List(
    Nutrient("calcium", "mg", minimum = Some(500), maximum = Some(2500)),
    Nutrient("carbohydrates", "g", minimum = Some(300), maximum = Some(950)),
    Nutrient("carotenealpha", "μg", minimum = None, maximum = None, enforce = false),
    Nutrient("carotenebeta", "μg", minimum = None, maximum = None, enforce = false),
    Nutrient("choline", "mg", minimum = Some(450), maximum = None),
    Nutrient("cholesterol", "mg", minimum = Some(135), maximum = Some(450)),
    Nutrient("copper", "mg", minimum = Some(0.7), maximum = Some(10), enforce = false),
    Nutrient("energy", "kcal", minimum = Some(3900), maximum = Some(4500)),
    Nutrient("fiber", "g", minimum = Some(35), maximum = Some(180)),
    Nutrient("iron", "mg", minimum = Some(10), maximum = Some(80)),
    Nutrient("magnesium","mg", minimum = Some(330), maximum = None),
    Nutrient("manganese","mg", minimum = Some(2.3), maximum = Some(11), enforce = false),
    Nutrient("potassium", "mg", minimum = Some(4000), maximum = Some(8000)),
    Nutrient("phosphorus", "mg", minimum = Some(480), maximum = Some(4000)),
    Nutrient("protein", "g", minimum = Some(50), maximum = None),
    Nutrient("retinol", "μg", minimum = None, maximum = Some(3000)),
    Nutrient("riboflavin", "mg", minimum = Some(1.1), maximum = None),
    Nutrient("saturatedfat", "g", minimum = Some(20), maximum = Some(55)),
    Nutrient("selenium", "μg", minimum = Some(45), maximum = Some(400), enforce = false),
    Nutrient("sodium", "mg", minimum = Some(1500), maximum = Some(5000)),
    Nutrient("sugar", "g", minimum = Some(12), maximum = Some(80)),
    Nutrient("totalfat", "g", minimum = Some(65), maximum = Some(130)),
    Nutrient("vitamina", "IU", minimum = Some(3000), maximum = None),
    Nutrient("vitaminb6", "mg", minimum = Some(1.1), maximum = Some(100), enforce = false),
    Nutrient("vitaminb12", "μg", minimum = Some(2.0), maximum = None),
    Nutrient("vitaminc", "mg", minimum = Some(40), maximum = Some(2000)),
    Nutrient("vitamind", "IU", minimum = Some(400), maximum = Some(4000), enforce = false),
    Nutrient("vitamine", "mg", minimum = Some(12), maximum = Some(1000), enforce = false),
    Nutrient("vitamink", "μg", minimum = Some(120), maximum = None, enforce = false),
    Nutrient("zinc", "mg", minimum = Some(9.4), maximum = Some(40))
  )

  val foods = List(
    getFood("Beef, carcass, separable lean and fat, select, raw", 0.01881, QuarterPound),
    getFood("Butter, without salt", 0.0275, EighthPound),
    getFood("Corn, white, steamed (Navajo)", 0.00183, BushelCorn),
    getFood("Wheat flour, whole-grain", 0.0037, SixteenthDryLitre, id = Some("flour")),
    getFood("Milk, whole, 3.25% milkfat, without added vitamin A and vitamin D", 0.003496, QuarterPound),
    getFood("Molasses", 0.014501, SixteenthOunce),
    getFood("Mutton, cooked, roasted (Navajo)", 0.025, EighthPound),
    getFood("Oats", 0.005, BushelOats),
    getFood("Peas, split, mature seeds, raw", 0.0041, BushelPeas),
    getFood("Pork, cured, bacon, raw", 0.02475, QuarterPound),
    getFood("Sweet potato, raw, unprepared", 0.0026, BushelPotatoes, id = Some("sweetpotatoes")),
    getFood("Potatoes, white, flesh and skin, raw", 0.00352, BushelPotatoes, id = Some("whitepotatoes"))
  ).flatMap(food => food match {
    case Some(food) => Some(food)
    case None =>
      logger.error("Couldn't find food")
      None
  })

  val proteinDiscount = 0.000002389
  val quantized = true

  def main (args: Array[String]) {

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

    logger.info("*** Model Parameters ***")
    logger.info(s"Quantized: ${if(quantized) "Y" else "N"}")
    logger.info(s"Protein Discount: ${proteinDiscount.formatted("%9.9f")}")
    logger.info("Prices:")
    foods.grouped(3).foreach { foodLine =>
      val priceStrs = foodLine.map(food => s"${(food.id + ":").padTo(15,' ')} ${food.cost.formatted("%5.5f")}")
      logger.info(priceStrs.mkString(" "))
    }
    logger.info("Disabled Foods: " + foods.filterNot(_.enabled).mkString(" "))
    logger.info("*** Model Results ***")
    logger.info(s"Adjusted Cost: ${result(0)}")
    logger.info(s"Actual Cost: $cost")
    logger.info(s"${"Food".padTo(20, ' ')} ${"Per Day".padTo(16, ' ')} ${"Per Year".padTo(16, ' ')}")
    optimalDiet.toSeq.sortBy(_._1.id).foreach { case (Food(foodId, _, _, unit, cookingCoef, _), amount) =>
      val perYearAmount = (amount * unit.unitToHundredGrams * Pound.hundredGramsToUnit * 365.25).toFloat
      logger.info(s"${foodId.padTo(20, ' ')} ${amount.toFloat.toString.padTo(11, ' ')} ${unit.name.padTo(4, ' ')} " +
      s"${perYearAmount.toString.padTo(11, ' ')} ${Pound.name.padTo(4, ' ')}")
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
      logger.info(s"${nutrient.id.padTo(15, ' ')} $enforcedInd ${value.formatted("%10.2f")} $unitInd\t$boundInd $bound")
    }
    mathematica.close()
  }

}

