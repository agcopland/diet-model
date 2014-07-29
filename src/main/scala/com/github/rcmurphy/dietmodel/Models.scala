package com.github.rcmurphy.dietmodel

case class ModelParams(
  name: String,
  foodSet: String,
  nutrientSet: String,
  priceSet: String,
  proteinDiscount: BigDecimal,
  quantized: Boolean)

case class Food(
  id: String,
  cost: Double,
  nutrients: Map[String, Double],
  unit: Unit,
  cookingCoef: Double = 1.0,
  enabled: Boolean = true)
/*{
  def this(id: String, cost: Double, dbId: String) = this(id, cost, )
}*/

case class Nutrient(id: String, unit: String, minimum: Option[Double], maximum: Option[Double], enforce: Boolean = true)
{
  if(maximum.isDefined && minimum.isDefined)
    require(maximum.get >= minimum.get)
}

case class Unit(name: String, grams: Double, maxSubdivisions: Int) {

  def subdivisionsToHundredGrams = grams / 100.0 / maxSubdivisions

  def hundredGramsToUnit = 100.0 / grams

  def unitToHundredGrams = grams / 100.0

  def subdivisionsToUnit = 1.0 / maxSubdivisions

}

object Unit {
  val byName = Map(
  "Kilogram" -> Unit("Kg", 1000.0, 1),
  "HundredGram" -> Unit("hg", 100.0, 100),

  "SixteenthDryLitre" -> Unit("l₁₆", 480, 16 * 7),

  "Pound" -> Unit("lb", 453.592, 1 * 7),
  "QuarterPound" -> Unit("lb₄", 453.592, 4 * 7),
  "EighthPound" -> Unit("lb₈", 453.592, 8 * 7),
  "Ounce" -> Unit("oz", 453.592, 16 * 7),
  "QuarterOunce" -> Unit("oz₄", 453.592, 4 * 16 * 7),
  "SixteenthOunce" -> Unit("oz₁₆", 453.592, 16 * 16 * 7),

  "BushelCorn" -> Unit("Bc", 56 * 453.592, 24 * 7),
  "BushelOats" -> Unit("Bo", 32 * 453.592, 24 * 7),
  "BushelPeas" -> Unit("Bpe", 25 * 453.592, 24 * 7),
  "BushelPotatoes" -> Unit("Bpo", 50 * 453.592, 24 * 7)
  )

}
