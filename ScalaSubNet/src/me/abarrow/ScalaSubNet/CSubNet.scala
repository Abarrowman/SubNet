package me.abarrow.ScalaSubNet

import scala.sys.process._
import me.abarrow.ScalaSubNet.utils.StringUtils

class CSubNet(val executablePath: String) {

  def train(trainingCSVPath: String, inputCols: Int, outputNetworkPath: String,
    layerSizes: Option[Traversable[Int]] = None, labelCols: Int = 0,
    algorithm:Option[String] = None, rounds:Option[Int] = None): String = {

    var options = Seq("-l=" + labelCols.toString())
    if (layerSizes.isDefined) {
      val layerSizeStr = "-s=" + layerSizes.get.mkString(",")
      options :+= layerSizeStr
    }
    if (algorithm.isDefined) {
      val algorithmStr = "-a=" + algorithm.get
      options :+= algorithmStr
    }
    if (rounds.isDefined) {
      val roundsStr = "-r=" + rounds.get
      options :+= roundsStr
    }
    
    val preParams = Seq(executablePath,
      "-t")
    val postParams = Seq(trainingCSVPath,
      inputCols.toString,
      outputNetworkPath)
    
    val command = Process(preParams ++ options ++ postParams)
    //println(command)
    val result = command.!
    if (result != 0) {
      throw new RuntimeException("Training network failed.")
    }
    outputNetworkPath
  }

  def execute(networkPath: String, inputCSVPath: String, outputCSVPath: String, labelCols: Int = 0): String = {
    val command = Process(Seq(
      executablePath,
      "-e",
      "-l=" + labelCols.toString(),
      networkPath,
      inputCSVPath,
      outputCSVPath
    ))
    //println(command)
    val result = command.!
    if (result != 0) {
      throw new RuntimeException("Executing network failed.")
    }
    outputCSVPath
  }

  def retrain(inputNetworkPath: String, retrainingCSVPath: String, outputNetworkPath: String,
    labelCols: Int = 0): String = {
    val command = Process(Seq(
        executablePath,
        "-r",
        "-l=" + labelCols.toString(),
        inputNetworkPath,
        retrainingCSVPath,
        outputNetworkPath
    ))
    //println(command)
    val result = command.!
    if (result != 0) {
      throw new RuntimeException("Retraining network failed.")
    }
    outputNetworkPath
  }

}