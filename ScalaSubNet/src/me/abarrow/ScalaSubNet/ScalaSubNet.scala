package me.abarrow.ScalaSubNet

import java.text.DecimalFormat
import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.File
import java.io.FileNotFoundException

import scala.io.StdIn
import scala.util.Random
import scala.sys.process._

import me.abarrow.ScalaSubNet.mal.MALStats
import me.abarrow.ScalaSubNet.mal.MALList
import me.abarrow.ScalaSubNet.mal.MALIDs
import me.abarrow.ScalaSubNet.mal.MALCSVSpec
import me.abarrow.ScalaSubNet.mal.MALEntry
import me.abarrow.ScalaSubNet.utils.FileUtils
import me.abarrow.ScalaSubNet.mal.MALImage

object ScalaSubNet {
  
  val decimalFormat = new DecimalFormat("#")
  decimalFormat.setMaximumFractionDigits(15)
  decimalFormat.setMaximumIntegerDigits(15)
  val dateFormat = new SimpleDateFormat("YYYY-MMM-dd HH:mm:ss.SSS")

  def getHumanTimestamp(): String = {
    val time = Calendar.getInstance().getTime()
    dateFormat.format(time)
  }

  def main(args: Array[String]): Unit = {
    //subMain()
    malMain()
  }

  def malMain(): Unit = {

    val testName = "malimage"
    val testFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\" + testName + "\\"
    FileUtils.createDirsIfNeeded(testFolder)
    //val trainingCSVPath = testFolder + "rated-list.csv"
    val sanityCSVPath = testFolder + "sanity-list.csv"
    val inputCSVPath = testFolder + "unrated-list.csv"
    val outputCSVPath = testFolder + "estimated-list.csv"
    val outputNetworkPath = testFolder + "network.net"
    

    val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\build\\Release\\CSubNet.exe")
    //val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\CSubNet\\x64\\Release\\CSubNet.exe")
    val convolvePath = "E:\\A\\Dropbox\\Dev\\C\\Maybe Eventually Add To Git\\Convolve\\Debug\\Convolve.exe"
    
    trainByImage(testFolder, convolvePath)
    
    //train by how the collective rated things based on their names
    //val top = MALStats.getTopAnime(10000)
    //val nameScores = getStatsArrayNameScores(top)
    //trainByName(nameScores, cSubNet, trainingCSVPath, outputNetworkPath)
    

    //train by genre
    //trainByGenre(trainingCSVPath, sanityCSVPath, inputCSVPath, cSubNet, outputNetworkPath, outputCSVPath)
  }

  def trainByImage(testFolder: String, convolvePath: String) = {
    val trainingCSVPath = testFolder + "rated-list.csv"
    val topAnimeCSVPath = testFolder + "top-list.csv"
    val largeImgFolder = testFolder + "largeImages"
    val scaleImgFolder = testFolder + "scaleImages"
    var topCSV:CSV = null
    try {
      topCSV = CSV.load(topAnimeCSVPath)
    } catch {
      case fnfe: FileNotFoundException => {
        val top = MALStats.getTopAnime(10000)
        topCSV = new CSV(Array("id", "name", "rating"))
        top.foreach { x =>
          if (x.rating.isDefined) {
            topCSV.addRow(Array(x.id.toString(), x.name, decimalFormat.format((x.rating.get - 1) / 9.0)))
          }
        }
        topCSV.save(topAnimeCSVPath);
      }
    }
    FileUtils.createDirsIfNeeded(largeImgFolder)
    FileUtils.createDirsIfNeeded(scaleImgFolder)
    val topMap = topCSV.toMaps()
    var count = 0
    val topWithImgMap = topMap.filter{ x =>
      val file = new File(largeImgFolder + File.separator + x("id") + ".jpg")
      var hasImg = true
      if (!file.exists()){
        //hasImg = false
        Thread.sleep(500)
        hasImg = MALImage.saveMainImage(x("id").toInt, file)
      }
      count += 1
      if (count % 50 == 0) {
        println("Saved images " + count + " / " + topMap.length)
      }
      hasImg
    }
    val outWidth = 11;
    val outHeight = 17;
    val widthRange = Range(0, outWidth);
    val heightRange = Range(0, outHeight);
    val pixelInputs = heightRange.map { y =>
      widthRange.map { x => 
        "rgb".map{ c =>
          "x" + x + "y" + y + c
        }
      }.flatten
    }.flatten
    val csvHeader = Array("id", "name") ++ pixelInputs ++ Array("rating")
    count = 0
    
    val trainCSV = new CSV(csvHeader)
    topWithImgMap.foreach{ x =>
      val ID = x("id")
      val largeImg = new File(largeImgFolder + File.separator + ID + ".jpg")
      val smallImg = new File(scaleImgFolder + File.separator + ID + ".png")
      val csvImg = new File(scaleImgFolder + File.separator + ID + ".csv")
      if (!smallImg.exists()){
        //val command = Process(Seq(convolvePath, largeImg.getAbsolutePath,
        //    "-grayscale", "-scale=" + outWidth + "x" + outHeight, smallImg.getAbsolutePath))  
        val command = Process(Seq(convolvePath, largeImg.getAbsolutePath,
            "-scale=" + outWidth + "x" + outHeight, smallImg.getAbsolutePath))
        val result = command.!
        if (result != 0) {
          throw new RuntimeException("Downscaling image failed.")
        }
      }
      if (!csvImg.exists()){
        val command = Process(Seq(convolvePath, smallImg.getAbsolutePath, csvImg.getAbsolutePath))
        val result = command.!
        if (result != 0) {
          throw new RuntimeException("Creating csv from image failed.")
        }
      }
      count += 1
      if (count % 50 == 0) {
        println("Downscaled images " + count + " / " + topWithImgMap.length)
      }
      val rowCSV = CSV.load(csvImg.getAbsolutePath)
      trainCSV.addRow(Array(ID, x("name")) ++ rowCSV.rows(0) ++ Array(x("rating")))
    }
    trainCSV.save(trainingCSVPath)
  }

  def trainByGenre(trainingCSVPath: String, sanityCSVPath:String,
      inputCSVPath: String, cSubNet: me.abarrow.ScalaSubNet.CSubNet,
      outputNetworkPath: String, outputCSVPath: String) = {
    
    val list = MALList.getListByUser("abarrow")
    
    //train by how I rated things based on their names
    //val nameScores = getListNameScores(list)
    //trainByName(nameScores, cSubNet, trainingCSVPath, training2CSVPath, outputNetworkPath, sanityCSVPath)
    
    var idx = 0
    val entriesAndStats = list.entries.map { x =>
      println(idx + " " + x.name)
      idx = idx + 1  
      val stats = MALStats.getStats(x.id)
      Thread.sleep(1000); //don't rate limit me
      (x, stats)
    }
    
    val ratedAnime = entriesAndStats.filter { x =>
      x._2.rating.isDefined
    }
    
    val scoredAnime = ratedAnime.filter { x =>
      (x._1.userStatus != 6) && (x._1.score != 0)
    }
    extractAnimeStatsCSV(scoredAnime, trainingCSVPath)
    
    val unScoredAnime = ratedAnime.filter { x =>
      x._1.score == 0
    }
    extractAnimeStatsCSV(unScoredAnime, inputCSVPath)
    val labelCols = 2
    val outputCols = 1
    val inputCols = CSV.load(trainingCSVPath).cols - labelCols - outputCols
    val rounds = 30000 //300000
    
    cSubNet.train(trainingCSVPath, inputCols, outputNetworkPath,
        None, labelCols = labelCols, algorithm = Some("backprop"),
        rounds = Some(rounds))
    cSubNet.execute(outputNetworkPath, inputCSVPath, outputCSVPath,
        labelCols = labelCols)
    cSubNet.execute(outputNetworkPath, trainingCSVPath, sanityCSVPath,
        labelCols = labelCols)
    
    
    println("Sanity")
    CSV.load(sanityCSVPath).toMaps().foreach { f =>
      val rating = f.get("userRating").get.toDouble * 9 + 1
      println(f.get("name").get + " -> " + rating); 
    }
    
    println("Estimates")
    CSV.load(outputCSVPath).toMaps().foreach { f =>
      val rating = f.get("userRating").get.toDouble * 9 + 1
      println(f.get("name").get + " -> " + rating); 
    }
  }

  val maxNameLen = 60
  val nameRange = 0 until maxNameLen
  val bitsPerChar = 6
  
  def nameToArr(name:String):IndexedSeq[Double] = {
    val alphaNumName = name.toLowerCase.filter { x => x.isDigit || x.isLetter }
    nameRange.map { x =>
      if (x < alphaNumName.size) {
        val cha = alphaNumName.charAt(x)
        if (cha.isDigit) {
          cha.toInt - 47 //'0' - '9' -> 1 - 10
        } else {
          cha.toInt - 86 //'a' - 'z' -> 11 - 36
        }
      } else {
        0
      }
    }.map { x =>
      x / 36.0
      //Array((x & 32) >>> 5, (x & 16) >>> 4, (x & 8) >>> 3, (x & 4) >>> 2, (x & 2) >>> 1, x & 1)
    }//.flatten
  }
  
  def getListNameScores(list:MALList):Array[(String,IndexedSeq[Double],Double)]  = {
    val filteredList = list.entries.filter { x => x.score != 0 }
    filteredList.map { x =>
      (x.name, nameToArr(x.name), (x.score - 1) / 9.0)
    }
  }
  
  def getStatsArrayNameScores(list:IndexedSeq[MALStats]):IndexedSeq[(String,IndexedSeq[Double],Double)]  = {
    val filteredList = list.filter { x => x.rating.isDefined }
    filteredList.map { x =>
      (x.name, nameToArr(x.name), (x.rating.get - 1) / 9.0)
    }
  }
  
  def trainByName(nameScores: IndexedSeq[(String,IndexedSeq[Double],Double)],
      cSubNet: CSubNet,  trainingCSVPath:String,
      outputNetworkPath:String) = {
    
    val bitRange = 0 until maxNameLen * bitsPerChar 
    val csvHeader = Array("name") ++ nameRange.map{x => "char" + x} ++ Array("score")
    //val csvHeader = Array("name") ++ bitRange.map{x => "b" + x} ++ Array("score")
    val trainCSV = new CSV(csvHeader)
    val csvRows = nameScores.map{x =>
      val nameArray = x._2.map{decimalFormat.format}
      //val nameArray = x._2.map{_.toString}
      
      Array(x._1) ++ nameArray ++  Array(decimalFormat.format(x._3))
    }
    csvRows.foreach { x => trainCSV.addRow(x) }
    trainCSV.save(trainingCSVPath)
  }

  def extractAnimeStatsCSV(entries: Array[(MALEntry, MALStats)], csvPath: String) = {
    
    
    
    val csv = MALCSVSpec.emptyCSV()
    entries.foreach { pair =>
      val entry = pair._1
      val stats = pair._2

      val leftRow = Array(entry.name, entry.id.toString(), decimalFormat.format((stats.rating.get - 1) / 9))
      val genreRow = MALIDs.genres.map(f =>
        if (stats.genres.get.contains(f._1)) {
          "1"
        } else {
          "0"
        }).toArray
      val studioRow = MALIDs.studios.map(f =>
        if (stats.studios.get.contains(f._1)) {
          "1"
        } else {
          "0"
        }).toArray
      val rightRow = Array(decimalFormat.format((entry.score - 1.0) / 9.0))
      csv.addRow(leftRow ++ genreRow ++ studioRow ++ rightRow)
      //csv.addRow(leftRow ++ genreRow ++ rightRow)
    }
    csv.save(csvPath)
  }

  def subMain(): Unit = {
    val wordContext = new WordContext("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\data\\wordlists",
        Set("adjectives", "nouns", "verbs", "adverbs", "articles",
      "pronouns", "prepositions", "postpositions", "pro_sentences", "conjunctions",
      "auxiliary_verbs", "interjections", "slang", "anime_slang"))

    val mkvToolNix = new MKVToolNix("C:\\Users\\Adam\\Downloads\\Portable Executables\\mkvtoolnix-64bit-8.6.1",
      "mkvinfo.exe", "mkvextract.exe")

    //val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\Debug\\CSubNet.exe")
    val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\CSubNet\\x64\\Release\\CSubNet.exe")

    //val inputVidFolder = "H:\\Favorites"
    val inputVidFolder = "H:\\Other\\Experiment"

    //val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-bake-sakura"
    //val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-1"
    //val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-experiment"
    val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-experiment-full"
    //val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-magnitude-8"

    val ctx = new VideoEstimator(wordContext, mkvToolNix, cSubNet, inputVidFolder, outputFolder)
    ctx.execute(3)
    //ctx.extractSubtitles()
    //ctx.calculateVideoStats()
  }
}