package me.abarrow.ScalaSubNet.mal

import me.abarrow.ScalaSubNet.CSV

object MALCSVSpec {
  val labelCSVColumns: Array[String] = Array("name","id")

  def inputCSVColumns: Array[String] = { 
    val ratingCols = Array("averageRating")
    val genreCols = MALIDs.genres.map(f => "genre_" + f._2).toArray
    val studioCols = MALIDs.studios.map(f => "studio_" + f._2).toArray
    (ratingCols ++ genreCols ++ studioCols)
    //ratingCols ++ genreCols
  }

  val outputCSVColumns: Array[String] = Array("userRating")

  def emptyCSV(): CSV = {
    new CSV(labelCSVColumns ++ inputCSVColumns ++ outputCSVColumns)
  }
}