package me.abarrow.ScalaSubNet

import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.File
import scala.io.StdIn
import me.abarrow.ScalaSubNet.mal.MALStats
import me.abarrow.ScalaSubNet.mal.MALList
import me.abarrow.ScalaSubNet.mal.MALIDs
import java.text.DecimalFormat


object ScalaSubNet {
  
  def getHumanTimestamp(): String = {
    val dateFormat = new SimpleDateFormat("YYYY-MMM-dd HH:mm:ss.SSS")
    val time = Calendar.getInstance().getTime()
    dateFormat.format(time)
  }

  def main(args: Array[String]):Unit = {
    //subMain()
    
    
    val csvPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\list-csv.csv"
    val outputNetworkPath = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\mal\\network.net"
    
    //val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\Debug\\CSubNet.exe")
    val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\CSubNet\\x64\\Release\\CSubNet.exe")
    
    println("Loading Anime List")
    val list = MALList.getList("abarrow")
    println(list.entries.mkString("\n"))
    
    //val stats = MALStats.getStats(9756)
    //println(stats)
    
    val leftCols = Array("name","id","averageRating")
    val genreCols = MALIDs.genres.map(f => "genre" + f).toArray
    val rightCols = Array("userRating")
    val header = leftCols ++ genreCols ++ rightCols
    
    val inputCols = genreCols.length + 1
    
    val decimalFormat = new DecimalFormat("#")
    decimalFormat.setMaximumFractionDigits(15)
    decimalFormat.setMaximumIntegerDigits(15)
    
    val csv = new CSV(header)
    var idx = 0
    println("Loading Stats")
    val entriesAndStats = list.entries.map { x =>
      println(idx + " " + x.name)
      idx = idx + 1
      val stats = MALStats.getStats(x.id)
      Thread.sleep(2000); //don't rate limit me
      (x, stats)
    }
    println("Filtering Stats")
    val filteredEntriesAndStats = entriesAndStats.filter{x =>
      x._2.rating.isDefined && (x._1.userStatus != 6) && (x._1.score != 0)
    }
    println("Assembling Training Data")
    filteredEntriesAndStats.foreach { pair =>
      val entry = pair._1
      val stats = pair._2
      
      val leftRow = Array(entry.name, entry.id.toString(), decimalFormat.format((stats.rating.get - 1) / 9))
      val genreRow = MALIDs.genres.map(f =>
        if(stats.genres.contains(f._1)) {
          "1"
        } else {
          "0"
        }
      ).toArray
      val rightRow = Array(decimalFormat.format((entry.score - 1.0) / 9.0))
      csv.addRow(leftRow ++ genreRow ++ rightRow)
    }
    println("Saving CSV")
    csv.save(csvPath)
    println("Training Network")
    cSubNet.train(csvPath, inputCols, outputNetworkPath, None, labelCols = 2)
  }
  
  
  
  
  def subMain():Unit = {
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