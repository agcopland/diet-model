package com.github.rcmurphy.dietmodel

import java.io.FileWriter

import com.github.rcmurphy.dietmodel.Database._

import scala.xml.{XML, NodeSeq}

object ModelRunner {

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
    val modelList = XML.loadFile(s"./data/models/models.xml")
    (modelList \ "optimalDietModel").flatMap { modelXml =>
      for {
        name <- (modelXml \ "@name").textOpt
        foodSet <- (modelXml \ "@foodSet").textOpt
        nutrientSet <- (modelXml \ "@nutrientSet").textOpt
        priceSet <- (modelXml \ "@priceSet").textOpt
        proteinDiscount <- (modelXml \ "@proteinDiscount").textOpt.map(BigDecimal(_))
        quantized <- (modelXml \ "@quantized").textOpt.map(_.toBoolean)
      } yield {
        logger.info(s"Loading optimal model '$name'")
        OptimalDietModelParams(name, foodSet, nutrientSet, priceSet, proteinDiscount, quantized)
      }
    }.toList ++
    (modelList \ "knownDietModel").flatMap { modelXml =>
      for {
        name <- (modelXml \ "@name").textOpt
        foodSet <- (modelXml \ "@foodSet").textOpt
        nutrientSet <- (modelXml \ "@nutrientSet").textOpt
        priceSet <- (modelXml \ "@priceSet").textOpt
        proteinDiscount <- (modelXml \ "@proteinDiscount").textOpt.map(BigDecimal(_))
        diet <- (modelXml \ "@diet").textOpt
      } yield {
        logger.info(s"Loading known model '$name'")
        KnownDietModelParams(name, foodSet, nutrientSet, priceSet, diet, proteinDiscount)
      }
    }.toList
  }
  def getKnownDiet(dietName: String): KnownDiet = {
    val xmlFile = XML.loadFile(s"./data/diets/$dietName.xml")
    val name = (xmlFile \ "@name").toString
    val amounts = (xmlFile \ "amount").flatMap { nutrientXml =>
      for {
        id <- (nutrientXml \ "@id").textOpt
        amount <- (nutrientXml \ "@poundsPerYear").textOpt.map(_.toDouble)
      } yield {
        (id, amount)
      }
    }.toMap
    KnownDiet(name, amounts)
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
    System.out.println("Running")
    val results = getModelParams.map {
      case modelParams: OptimalDietModelParams =>
        val priceSet = getPrices(modelParams.priceSet)
        val result = new OptimalModel(
          modelParams.name,
          getFoods(modelParams.foodSet, priceSet),
          getNutrients(modelParams.nutrientSet),
          modelParams.proteinDiscount,
          modelParams.quantized
        ).run()
        (modelParams.name, result)
      case modelParams: KnownDietModelParams =>
        val priceSet = getPrices(modelParams.priceSet)
        val result = new KnownDietModel(
          modelParams.name,
          getFoods(modelParams.foodSet, priceSet),
          getNutrients(modelParams.nutrientSet),
          getKnownDiet(modelParams.diet),
          modelParams.proteinDiscount
        ).run()
        (modelParams.name, result)
    }
    results.foreach {
      case (file, result) => writeToFile(s"output/model-${file}-result.txt", result)
    }

  }

}
