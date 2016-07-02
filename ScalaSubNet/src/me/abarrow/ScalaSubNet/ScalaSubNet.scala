package me.abarrow.ScalaSubNet

import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.File
import scala.io.StdIn
import me.abarrow.ScalaSubNet.mal.MALStats
import me.abarrow.ScalaSubNet.mal.MALList
import me.abarrow.ScalaSubNet.mal.MALIDs
import me.abarrow.ScalaSubNet.mal.MALCSVSpec
import me.abarrow.ScalaSubNet.mal.MALEntry
import java.text.DecimalFormat

object ScalaSubNet {

  def getHumanTimestamp(): String = {
    val dateFormat = new SimpleDateFormat("YYYY-MMM-dd HH:mm:ss.SSS")
    val time = Calendar.getInstance().getTime()
    dateFormat.format(time)
  }

  def main(args: Array[String]): Unit = {
    //subMain()
    malMain()
  }

  def malMain(): Unit = {

    val trainingCSVPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\rated-list.csv"
    val inputCSVPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\unrated-list.csv"
    val outputCSVPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\estimated-list.csv"
    val sanityCSVPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\sanity-list.csv"
    val outputNetworkPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\network.net"

    //val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\Debug\\CSubNet.exe")
    val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\CSubNet\\x64\\Release\\CSubNet.exe")


    /*val list = MALList.getList("abarrow")
    println(list.entries.mkString("\n"))

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
     (x._1.userStatus == 6) && (x._1.score == 0)
    }
    extractAnimeStatsCSV(unScoredAnime, inputCSVPath)

    val inputCols = MALCSVSpec.inputCSVColumns 
    cSubNet.train(trainingCSVPath, inputCols.length, outputNetworkPath, None, labelCols = 2, Some("anneal"))
    cSubNet.execute(outputNetworkPath, inputCSVPath, outputCSVPath, labelCols = 2)
    cSubNet.execute(outputNetworkPath, trainingCSVPath, sanityCSVPath, labelCols = 2)
    */
    
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

  def extractAnimeStatsCSV(entries: Array[(MALEntry, MALStats)], csvPath: String) = {
    
    val decimalFormat = new DecimalFormat("#")
    decimalFormat.setMaximumFractionDigits(15)
    decimalFormat.setMaximumIntegerDigits(15)
    
    val csv = MALCSVSpec.emptyCSV()
    entries.foreach { pair =>
      val entry = pair._1
      val stats = pair._2

      val leftRow = Array(entry.name, entry.id.toString(), decimalFormat.format((stats.rating.get - 1) / 9))
      val genreRow = MALIDs.genres.map(f =>
        if (stats.genres.contains(f._1)) {
          "1"
        } else {
          "0"
        }).toArray
      val studioRow = MALIDs.studios.map(f =>
        if (stats.studios.contains(f._1)) {
          "1"
        } else {
          "0"
        }).toArray
      val rightRow = Array(decimalFormat.format((entry.score - 1.0) / 9.0))
      csv.addRow(leftRow ++ genreRow ++ studioRow ++ rightRow)
    }
    csv.save(csvPath)
  }

  def subMain(): Unit = {
    val wordContext = new WordContext("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\data\\wordlists", Set("adjectives", "nouns", "verbs", "adverbs", "articles",
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