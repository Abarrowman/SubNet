package me.abarrow.ScalaSubNet.mal

import collection._

object MALIDs {
  //many studios are missing the list of producers is way too long
  val studios = mutable.Map.empty[Int, String]
    
  val genres = immutable.Map(
      1 -> "Action",
      2 -> "Adventure",
      3 -> "Cars",
      4 -> "Comedy",
      5 -> "Dementia",
      6 -> "Demons",
      7 -> "Mystery",
      8 -> "Drama",
      9 -> "Ecchi",
      10 -> "Fantasy",
      11 -> "Game",
      12 -> "Hentai",
      13 -> "Historical",
      14 -> "Horror",
      15 -> "Kids",
      16 -> "Magic",
      17 -> "Martial Arts",
      18 -> "Mecha",
      19 -> "Music",
      20 -> "Parody",
      21 -> "Samurai",
      22 -> "Romance",
      23 -> "School",
      24 -> "Sci-Fi",
      25 -> "Shoujo",
      26 -> "Shoujo Ai",
      27 -> "Shounen",
      28 -> "Shounen Ai",
      29 -> "Space",
      30 -> "Sports",
      31 -> "Super Power",
      32 -> "Vampire",
      33 -> "Yaoi",
      34 -> "Yuri",
      35 -> "Harem",
      36 -> "Slice of Life",
      37 -> "Supernatural",
      38 -> "Military",
      39 -> "Police",
      40 -> "Psychological",
      41 -> "Thriller",
      42 -> "Seinen",
      43 -> "Josei")
      
  val statuses = immutable.Map(
      1 -> "Watching",
      2 -> "Completed",
      3 -> "On Hold",
      4 -> "Dropped",
      6 -> "Plan to Watch"
  )
}