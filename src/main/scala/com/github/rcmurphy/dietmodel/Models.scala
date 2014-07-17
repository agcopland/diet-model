package com.github.rcmurphy.dietmodel

import java.io.FileReader

import au.com.bytecode.opencsv.CSVReader
import org.slf4j.LoggerFactory


case class Food(id: String, cost: Double, nutrients: Map[String, Double], unit: Unit, cookingCoef: Double = 1.0)
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
  val Kilogram = Unit("Kg", 1000.0, 1)
  val HundredGram = Unit("hg", 100.0, 100)

  val SixteenthDryLitre = Unit("l₁₆", 480, 16 * 7)

  val Pound = Unit("lb", 453.592, 1 * 7)
  val QuarterPound = Unit("lb₄", 453.592, 4 * 7)
  val EighthPound = Unit("lb₈", 453.592, 8 * 7)

  val BushelCorn = Unit("Bc", 56 * 453.592, 16 * 7)
  val BushelOats = Unit("Bo", 32 * 453.592, 16 * 7)
  val BushelPeas = Unit("Bpe", 25 * 453.592, 16 * 7)
  val BushelPotatoes = Unit("Bpo", 50 * 453.592, 16 * 7)

}
