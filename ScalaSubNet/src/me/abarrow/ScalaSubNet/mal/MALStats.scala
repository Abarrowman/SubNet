package me.abarrow.ScalaSubNet.mal

import org.jsoup.Jsoup
import collection.JavaConverters._
import scala.collection.mutable.Map
import java.net.URL

class MALStats(val id:Int, val name:String,
    val rating:Option[Double], val genres:Option[Set[Int]], val studios:Option[Set[Int]]) {
  override def toString():String = {
    val nameRating = if (rating.isDefined) {
      id.toString() + ". " + name + " (" + rating.get + "/10)" 
    } else {
      id.toString() + ". " +name + " (Unrated)"
    }
    val genreRating = if (genres.isDefined) {
      "Genres:[" + genres.get.map(MALIDs.genres).mkString(",") + "]"
    } else {
      "Genres:Unknown"
    }
    val studioRating = if (studios.isDefined) {
      "Studios:[" + studios.get.map(MALIDs.studios).mkString(",") + "]"
    } else {
      "Studios:Unknown"
    }
    Array(nameRating, genreRating, studioRating).reduce{(a, b) => a + " " +  b}
  }
}

object MALStats {
  private val bufferedStats = scala.collection.mutable.Map[Int, MALStats]()
  
  def getTopAnime(count:Int = 1000):IndexedSeq[MALStats] = {
    Range(0, count, 50).map{idx =>
      Thread.sleep(100)
      val doc = Jsoup.parse(new URL(MALURLs.MAL_TOP_ANIME_PAGE_PREFIX + idx.toString()), 60000)
      println("Loaded " + (idx + 50) + "/" + count)
      val rankLists = doc.select("tr.ranking-list")
      rankLists.asScala.map { rl =>
        val titleLink = rl.select("td.title div.detail a").first()
        val id = titleLink.attr("href").split("/")(4).toInt
        val name = titleLink.html()
        val scoreStr = rl.select("td.score span").first().html()
        val score = if (scoreStr == "N/A") {
          None
        }else {
          Some(scoreStr.toDouble)
        }
        new MALStats(id, name, score, None, None)
      }
    }.flatten
  }
  
  def getStats(animeId:Int):MALStats = {
    val statsOption = bufferedStats.get(animeId)
    if (statsOption.isDefined) {
      statsOption.get
    } else {
      val doc = Jsoup.parse(new URL(MALURLs.MAL_ANIME_PAGE_PREFIX + animeId.toString()), 60000)
      
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
      
      val stats = new MALStats(animeId, name, rating, Some(genreIDs.toSet), Some(studioIDs.toSet))
      bufferedStats(animeId) = stats
      stats
    }
  }
  
  
}