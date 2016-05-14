package me.abarrow.ScalaSubNet

import java.text.SimpleDateFormat
import java.util.Calendar
import java.io.File
import scala.io.StdIn

object ScalaSubNet {
  
  def getHumanTimestamp(): String = {
    val dateFormat = new SimpleDateFormat("YYYY-MMM-dd HH:mm:ss.SSS")
    val time = Calendar.getInstance().getTime()
    dateFormat.format(time)
  }

  def main(args: Array[String]):Unit = {
    val wordContext = new WordContext("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\data\\wordlists", Set("adjectives", "nouns", "verbs", "adverbs", "articles",
    "pronouns", "prepositions", "postpositions", "pro_sentences", "conjunctions",
    "auxiliary_verbs", "interjections", "slang", "anime_slang"))
    
    val mkvToolNix = new MKVToolNix("C:\\Users\\Adam\\Downloads\\Portable Executables\\mkvtoolnix-64bit-8.6.1",
      "mkvinfo.exe", "mkvextract.exe")
    //val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\Debug\\CSubNet.exe")
    val cSubNet = new CSubNet("E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\CSubNet\\CSubNet\\x64\\Release\\CSubNet.exe")
    val inputVidFolder = "H:\\Favorites"
    //val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-bake-sakura"
    val outputFolder = "E:\\A\\Dropbox\\Dev\\Multiple\\SubNet\\test\\test-1"

    val ctx = new VideoEstimator(wordContext, mkvToolNix, cSubNet, inputVidFolder, outputFolder)
    ctx.execute(3);
  }
  
  

}