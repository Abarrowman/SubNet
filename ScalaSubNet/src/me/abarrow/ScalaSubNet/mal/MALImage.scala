package me.abarrow.ScalaSubNet.mal

import java.io.File
import java.io.FileOutputStream
import java.net.URL
import org.jsoup.Jsoup
import org.jsoup.parser.Parser
import java.nio.channels.Channels

object MALImage {
  def saveMainImage(animeID:Int, imagePath:File):Boolean = {
    val doc = Jsoup.parse(new URL(MALURLs.MAL_ANIME_PAGE_PREFIX + animeID.toString()), 60000)
    val mainImage = doc.select("img.ac").first()
    if (mainImage == null) {
      return false
    }
    val imgSrc = mainImage.attr("src")
    val rbc = Channels.newChannel(new URL(imgSrc).openStream())
    val fos = new FileOutputStream(imagePath)
    try {
      fos.getChannel().transferFrom(rbc, 0, Long.MaxValue)
    } finally {
      fos.close()
      rbc.close()
    }
    true
  }
}