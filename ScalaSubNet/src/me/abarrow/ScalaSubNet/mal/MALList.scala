package me.abarrow.ScalaSubNet.mal

import org.jsoup.Jsoup
import org.jsoup.parser.Parser 
import collection.JavaConverters._


class MALList (val entries:Array[MALEntry]) {
  
}

object MALList {
  private val MAL_LIST_PREFIX = "http://myanimelist.net/malappinfo.php?u="
  private val MAL_LIST_SUFFIX = "&status=all&type=anime"
  private val xmlParser = Parser.xmlParser()
  def getList(userId:String):MALList = {
    val listXML = Jsoup.connect(MAL_LIST_PREFIX + userId + MAL_LIST_SUFFIX).parser(xmlParser).get()
    
    new MALList(listXML.getElementsByTag("anime").asScala.map { x =>
      val id = x.getElementsByTag("series_animedb_id").first().html().toInt
      val name = x.getElementsByTag("series_title").first().html()
      val score = x.getElementsByTag("my_score").first().html().toInt
      val status = x.getElementsByTag("my_status").first().html().toInt
      new MALEntry(id, name, score, status)
    }.toArray)
  }
}