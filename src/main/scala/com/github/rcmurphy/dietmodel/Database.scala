package com.github.rcmurphy.dietmodel

import java.io.FileReader

import au.com.bytecode.opencsv.CSVReader
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._


object Database {
  def logger = LoggerFactory.getLogger(getClass)

    logger.info("Loading Foods...")
    val foodIds = readFoods()
    logger.info(s"${foodIds.size} food(s) loaded")
    logger.info("Loading Nutrients...")
    val nutrients = readNutrients()
    logger.info(s"${nutrients.size} nutrient(s) loaded")
    logger.info("Loading Nutrient / Food Associations...")
    val foodNutrientsById = readFoodNutrients
    logger.info(s"${foodNutrientsById.size} record(s) loaded")
    logger.info(s"Grouping Nutrient / Food Associations...")
    val foodNutrientsByFoodId = foodNutrientsById
      .groupBy { case ((food, _), _) => food }
      .mapValues { _.map{ case ((_, nutrient), amount) => (nutrient, amount) }.toMap }
    val foods = foodIds.map { case DBFoodId(id, name) =>
      val foodNutrientByIds = foodNutrientsByFoodId.getOrElse(id, {
        logger.warn(s"Can't retrieve Nutrient Table: $id, $name")
        Map[Int, Double]()
      })
      val foodNutrients = nutrients.map(nutrient => (nutrient, foodNutrientByIds.getOrElse(nutrient.id, 0.0))).toMap
      DBFood(id, name, foodNutrients)
    }
  logger.info(s"Done Loading DBs...")

  /*def getFood(dbId: Int, id: Option[String] = None, cost: Double): Option[Food] = {
    val food = foods.find(_.id == dbId).map(dbFoodToFood(_, id, cost))
    food.map(food => logger.debug(s"Retrieved Food: $food"))
    food
  }*/
  def getFood(
    id: String,
    dbName: String,
    cost: Double,
    unit: Unit = Unit.byName("HundredGram"),
    cookingCoef: Double = 1.0,
    enabled: Boolean = true): Option[Food] = {
    val food = foods.find(_.name == dbName).map(dbFoodToFood(_, id, cost, unit, cookingCoef, enabled))
    food.map(food => logger.debug(s"Retrieved Food: $food"))
    food
  }

  protected def dbFoodToFood(
    food: DBFood,
    id: String,
    cost: Double, unit: Unit,
    cookingCoef: Double = 1.0,
    enabled: Boolean = true): Food = {
    Food(
      id = id,
      cost = cost,
      nutrients = food.nutrients.flatMap {
        case (dbNutrient, amount) if saneNutrientName.isDefinedAt(dbNutrient.shortName) =>
          Some((saneNutrientName(dbNutrient.shortName), amount / cookingCoef))
        case _ => None
      }.toMap,
      unit = unit,
      cookingCoef = cookingCoef,
      enabled = enabled
    )
  }

  protected def saneNutrientName: PartialFunction[String, String] = {

    case "CA" => "calcium"
    case "CARTA" => "carotenealpha"
    case "CARTB" => "carotenebeta"
    case "CU" => "copper"
    case "CHOCDF" => "carbohydrates"
    case "CHOLE" => "cholesterol"
    case "CHOLN" => "choline"
    case "ENERC_KCAL" => "energy"
    case "FASAT" => "saturatedfat"
    case "FAT" => "totalfat"
    case "FE" => "iron"
    case "FIBTG" => "fiber"
    case "K" => "potassium"
    case "MG" => "magnesium"
    case "MN" => "manganese"
    case "NA" => "sodium"
    case "P" => "phosphorus"
    case "PROCNT" => "protein"
    case "RETOL" => "retinol"
    case "RIBF" => "riboflavin"
    case "SE" => "selenium"
    case "SUGAR" => "sugar"
    case "VITA_IU" => "vitamina"
    case "VITB6A" => "vitaminb6"
    case "VITB12" => "vitaminb12"
    case "VITC" => "vitaminc"
    case "VITD" => "vitamind"
    case "TOCPHA" => "vitamine"
    case "VITK" => "vitamink"
    case "ZN" => "zinc"

  }




  protected def readFoodNutrients(): Map[(Int, Int), Double] = {
    val foodNutrientReader = new CSVReader(new FileReader("./data/foods/NUT_DATA.txt"), '^', '~')
    val foodNutrientsById = foodNutrientReader.readAll().map { foodNutrient =>
      val foodIdNum = foodNutrient(0).replace("~", "").toInt
      val nutrientIdNum = foodNutrient(1).replace("~", "").toInt
      val value = foodNutrient(2).replace("~", "").toDouble
      //logger.debug(food.mkString(","))
      ((foodIdNum, nutrientIdNum), value)
    }.toMap
    foodNutrientReader.close()
    foodNutrientsById
  }

  protected def readFoods(): List[DBFoodId] = {
    val foodReader = new CSVReader(new FileReader("./data/foods/FOOD_DES.txt"), '^', '~')
    val foods = foodReader.readAll().map { food =>
      val idNum = food(0).replace("~", "").toInt
      val name = food(2).replace("~", "")
      DBFoodId(idNum, name)
    }.toList
    foodReader.close()
    foods
  }

  protected def readNutrients(): List[DBNutrient] = {
    val nutrientReader = new CSVReader(new FileReader("./data/foods/NUTR_DEF.txt"), '^', '~')
    val nutrients = nutrientReader.readAll().map { nutrient =>
      val idNum = nutrient(0).replace("~", "").toInt
      val unit = nutrient(1).replace("~", "")
      val shortName = nutrient(2).replace("~", "")
      val longName = nutrient(3).replace("~", "")
      logger.trace(s"Loaded Nutrient (id: $idNum, name: '$shortName' / '$longName', unit: $unit)")
      DBNutrient(idNum, shortName, longName, unit)
    }.toList
    nutrientReader.close()
    nutrients
  }


}

case class DBNutrient(id: Int, shortName: String, longName: String, unit: String)
case class DBFoodId(id: Int, name: String)
case class DBFood(id: Int, name: String, nutrients: Map[DBNutrient, Double])
