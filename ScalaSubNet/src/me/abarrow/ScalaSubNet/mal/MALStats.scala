package me.abarrow.ScalaSubNet.mal

import org.jsoup.Jsoup
import collection.JavaConverters._
import scala.collection.mutable.Map
import java.net.URL

class MALStats(val name:String, val rating:Option[Double], val genres:Set[Int], val studios:Set[Int]) {
  override def toString():String = {
    if (rating.isDefined) {
      name + " (" + rating.get + "/10) Genres:[" + genres.map(MALIDs.genres).mkString(",") +
      "] Studios:[" + studios.map(MALIDs.studios).mkString(",") + "]" 
    } else {
      name + " (Unrated) Genres:[" + genres.map(MALIDs.genres).mkString(",") +
      "] Studios:[" + studios.map(MALIDs.studios).mkString(",") + "]"
    }
  }
}

object MALStats {
  private val MAL_ANIME_PAGE_PREFIX = "http://myanimelist.net/anime/"
  
  private val bufferedStats = scala.collection.mutable.Map[Int, MALStats]()
  
  def getStats(animeId:Int):MALStats = {
    val statsOption = bufferedStats.get(animeId)
    if (statsOption.isDefined) {
      statsOption.get
    } else {
      val doc = Jsoup.parse(new URL(MAL_ANIME_PAGE_PREFIX + animeId.toString()), 60000)
      //val doc = Jsoup.connect(MAL_ANIME_PAGE_PREFIX + animeId.toString()).get()
      
      val nameElement = doc.select("h1 span[itemprop=name]").first()
      if (nameElement == null) {
        throw new IllegalArgumentException("Anime on MAL must have names.")
      }
      val name = nameElement.html()
      
      val ratingElement = doc.select("span[itemprop=ratingValue]").first()
      val rating = if (ratingElement == null) {
        None
      } else {
        val ratingStr = ratingElement.html();
        if (ratingStr == "N/A") {
          None
        }else {
          Some(ratingStr.toDouble)
        }
      }
      
      val genres = doc.select("div a[href^='/anime/genre/']")
      val genreIDs = genres.asScala.map{ x =>
        //given href of the form /anime/genre/ID/name
        x.attr("href").split("/")(3).toInt
      }.filter(MALIDs.genres.contains)
      
      val studioLabel = doc.select("span:containsOwn(Studios:)").first()
      if (studioLabel == null) {
        throw new IllegalArgumentException("Anime on MAL must have studios.")
      }
      
      val producers = studioLabel.parent().select("a[href^='/anime/producer/']")
      val studioIDs = producers.asScala.map{ x =>
        //given href of the form /anime/producer/ID/name
        val id = x.attr("href").split("/")(3).toInt
        val name = x.html()
        MALIDs.studios(id) = name
        id
      }
      
      val stats = new MALStats(name, rating, genreIDs.toSet, studioIDs.toSet)
      bufferedStats(animeId) = stats
      stats
    }
  }
  
  
}