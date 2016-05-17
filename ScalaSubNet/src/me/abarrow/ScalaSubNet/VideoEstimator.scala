package me.abarrow.ScalaSubNet

import scala.collection.mutable
import scala.io.StdIn
import java.io.File
import java.text.DecimalFormat
import me.abarrow.ScalaSubNet.utils.FileUtils


class VideoEstimator (val wordContext:WordContext, val mkvToolNix:MKVToolNix,
  val cSubNet:CSubNet, val inputVidFolder:String, val outputFolder:String){
  
  val videoCSVSpec = new VideoCSVSpec(wordContext)
  
  val unknownWords:mutable.HashSet[String] = mutable.HashSet[String]()
  val outputFolderFile:File = new File(outputFolder)
  FileUtils.createDirsIfNeeded(outputFolder)
  
  
  val ratingsPath = new File(outputFolderFile, "ratings.csv").getAbsolutePath
  val subsFolder = new File(outputFolderFile, "subs")
  val subsPath = new File(outputFolderFile, "subs.csv").getAbsolutePath
  val ratedVidsPath = new File(outputFolderFile, "rated.csv").getAbsolutePath
  val unratedVidsPath = new File(outputFolderFile, "unrated.csv").getAbsolutePath
  val estimatedVidsPath = new File(outputFolderFile, "estimates.csv").getAbsolutePath
  val sanityVidsPath = new File(outputFolderFile, "sanity.csv").getAbsolutePath
  val networkPath = new File(outputFolderFile, "network.net").getAbsolutePath
  val unknownWordsPath = new File(outputFolderFile, "unknown_words.txt").getAbsolutePath
  
  def execute(startingStage:Int = 0): Unit = {
    if (startingStage <= 0) {
      createRatingList()
    }
    if (startingStage <= 1) {
      extractSubtitles()
    }
    if (startingStage <= 2) {
      calculateVideoStats()
    }
    if (startingStage <= 3) {
      trainNetwork()
    }
    if (startingStage <= 4) {
      executeNetwork()
    }
    showRatings()
  }
  
  def createRatingList():Unit = {
    val listCSV = new CSV(Array("path", "rating"))
    val videosAndRatings = getVideosCommandLine(inputVidFolder).foreach{x => 
      listCSV.addRow(Array(x._1, x._2.toString()))
    }
    listCSV.save(ratingsPath)
  }
  
  def extractSubtitles():Unit = {
    val subCSV = new CSV(Array("path", "text", "rating"))
    FileUtils.createDirsIfNeeded(subsFolder.getAbsolutePath)
    val ratingsCSV = CSV.load(ratingsPath).toMaps().foreach({ m =>
      val path = m("path")
      val ratingStr = m("rating")
      val rating = ratingStr.toInt
      if (VideoUtils.isMKVVideo(path) && (rating >= 0) && (rating <= 10)) {
        val subtitles = mkvToolNix.extractBestSubTrack(path)
        if (subtitles.isDefined) {
          val videoNameAndType = FileUtils.getFileNameAndExtension(path)
          val destFile = File.createTempFile(videoNameAndType._1, ".txt", subsFolder) 
          FileUtils.saveUTF8File(destFile, ASSParser.parse(subtitles.get))
          subCSV.addRow(Array(path, destFile.getAbsolutePath, ratingStr))
        }
      }
    })
    subCSV.save(subsPath)
  }
  
  def calculateVideoStats():Unit = {
    val videosAndRatings = CSV.load(subsPath).toMaps().map{m =>
      (m("path"), m("text"), m("rating").toInt)
    }.filter { x => (x._3 >= 0) && (x._3 <= 10) }.groupBy { x => x._3 > 0 }
    
    computeVideoData(videosAndRatings.getOrElse(true, Array()), ratedVidsPath)
    computeVideoData(videosAndRatings.getOrElse(false, Array()), unratedVidsPath)
    
    FileUtils.saveUTF8Text(unknownWordsPath, unknownWords.mkString("\n"))
  }
  
   private def computeVideoData(videos: Array[(String, String, Int)], outputCSVPath: String): Unit = {
    val csv = videoCSVSpec.emptySubtitleStatsCSV
    videos.foreach { x =>
      addTextStats(csv, x._1, FileUtils.openUTF8Text(x._2), x._3)
    }
    csv.save(outputCSVPath);
  }
  
  def trainNetwork():Unit = {
    val labelColumns = videoCSVSpec.labelCSVColumns.size
    cSubNet.train(ratedVidsPath, videoCSVSpec.inputCSVColumns.size, networkPath, None, labelCols = labelColumns)
  }
  
  def executeNetwork():Unit = {
    val labelColumns = videoCSVSpec.labelCSVColumns.size
    cSubNet.execute(networkPath, unratedVidsPath, estimatedVidsPath, labelCols = labelColumns)
    cSubNet.execute(networkPath, ratedVidsPath, sanityVidsPath, labelCols = labelColumns)
  }
  
  def showRatings():Unit = {
    println("Sanity")
    CSV.load(sanityVidsPath).toMaps().foreach { f =>
      val rating = (videoCSVSpec.outputCSVColumns.map { x => f.get(x).get.toDouble }.reduce { (x, y) => x + y } * 9) + 1
      println(f.get("name").get + " -> " + rating); 
    }
    println("Estimates")
    CSV.load(estimatedVidsPath).toMaps().foreach { f =>
      val rating = (videoCSVSpec.outputCSVColumns.map { x => f.get(x).get.toDouble }.reduce { (x, y) => x + y } * 9) + 1
      println(f.get("name").get + " -> " + rating); 
    }
  }
  
  
  private def addVideoStats(csv: CSV, targetFilePath: String, rating: Int): Unit = {
    val subtitles = mkvToolNix.extractBestSubTrack(targetFilePath)
    if (subtitles.isDefined) {
    }
  }
  
  private def getVideosCommandLine(folder: String): Array[(String, Int)] = {
    val help = () => {
      println("Rate each item from 1 (worst) to 10 (best)")
      println("* : Videos in the folder have different ratings")
      println("0 or ? : The video or folder contains videos that haven't been seen")
      println("! : Ignore")
    }
    help()
    FileUtils.evaluateDeepFilesInFolder(folder, { x =>
      var result: Option[Int] = None;
      var parsing = x.isDirectory() || VideoUtils.isMKVVideo(x.getAbsolutePath)
      while (parsing) {
        print(x + " Rating: ")
        val input = StdIn.readLine();
        if (x.isDirectory() && input == "*") { //ignore ratings
          parsing = false
        } else if (input == "?") { //unrated
          parsing = false
          result = Some(0)
        } else if (input == "!") { //skip
          parsing = false
          result = Some(-1)
        } else {
          try {
            val inputInt = input.toInt
            if (inputInt >= 0 && inputInt <= 10) {
              result = Some(inputInt)
              parsing = false
            } else {
              help()
            }
          } catch {
            case nfe: java.lang.NumberFormatException => help()
          }
        }
      }
      result
    }).filter { x => x._2 >= 0 && VideoUtils.isMKVVideo(x._1) }
  }
  
  private def addTextStats(csv: CSV, name: String, textContents: String, rating: Int): Unit = {
    val rawWords: Array[String] = FileUtils.splitApartWords(textContents)
    val qwords: Array[QualifiedWord] = rawWords.map { x => QualifiedWord(x) }.filter { x => x != null }
    val words: Array[String] = qwords.map { x => x.word }

    if (words.length == 0) {
      return
    }

    var sentenceIdx = 0
    val sentences: Map[Int, Array[QualifiedWord]] = qwords.groupBy { x =>
      val oldIdx = sentenceIdx
      if (x.endsSentence) {
        sentenceIdx += 1
      }
      oldIdx
    }
    val numSentences = sentenceIdx
    if (numSentences == 0) {
      return
    }
    val numQuestions = sentences.count(p => p._2.last.punct == Some('?'))

    val wordLengthDist = words.groupBy { x => x.length() }.map { f => (f._1, f._2.size) }
    val sentenceLengthDist = sentences.groupBy { x => x._2.length }.map { f => (f._1, f._2.size) }
    val avgSentenceLength = words.length.toDouble / numSentences
    val avgWordLength = words.map { x => x.length() }.reduce { (a, b) => a + b }.doubleValue() / words.length
    val uniqueWords = words.toSet

    val groupedWords = uniqueWords.groupBy { x =>
      val result = wordContext.wordList.find { y => y._2(x) }
      if (result.isDefined) result.get._1 else wordContext.unknownWordType
    }

    val otherWords = groupedWords.getOrElse(wordContext.unknownWordType, Set())

    var decimalFormat = new DecimalFormat("#")
    decimalFormat.setMaximumFractionDigits(15)
    decimalFormat.setMaximumIntegerDigits(15)

    val mainEntries = Map[String, String]("name" -> name,
      "avg_sentence_length" -> decimalFormat.format(avgSentenceLength),
      "avg_word_length" -> decimalFormat.format(avgWordLength),
      "unique_ratio" -> decimalFormat.format(uniqueWords.size.doubleValue() / words.size),
      "question_ratio" -> decimalFormat.format(numQuestions.doubleValue() / numSentences))

    val wordTypeEntries = wordContext.wordTypesAndUnknown.map { f =>
      f + "_ratio" -> decimalFormat.format(groupedWords.getOrElse(f, Set()).size.doubleValue() / uniqueWords.size)
    }.toMap

    //val ratingEntries = (2 to 10).map { x => ("rated_ge_" + x, if (rating >= x) "1" else "0") }.toMap
    val decimalRating = (rating - 1.0d) / 9;
    val ratingEntries = Map[String,String]("rating" -> decimalFormat.format(decimalRating))
    
    csv.addRow(mainEntries ++ wordTypeEntries ++ ratingEntries)

    //println(name + " Content:")
    //println(textContents)
    //println(otherWords.mkString("\n"))
    unknownWords ++= otherWords
  }
}