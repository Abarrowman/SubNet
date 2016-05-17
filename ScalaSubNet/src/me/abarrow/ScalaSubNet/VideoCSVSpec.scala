package me.abarrow.ScalaSubNet

class VideoCSVSpec(val wordContext:WordContext, val ratingRange:Range.Inclusive = 2 to 10) {
  
  val labelCSVColumns: Array[String] = Array("name")

  val inputCSVColumns: Array[String] = {
    val mainColumns = Array("avg_sentence_length", "avg_word_length", "unique_ratio",
      "question_ratio")
    val wordTypeColumns = wordContext.wordTypesAndUnknown.map { x => x + "_ratio" }
    mainColumns ++ wordTypeColumns
  }

  val outputCSVColumns: Array[String] = {
    Array("rating")
    //ratingRange.map { x => "rated_ge_" + x }.toArray
  }

  def emptySubtitleStatsCSV: CSV = {
    new CSV(labelCSVColumns ++ inputCSVColumns ++ outputCSVColumns)
  }
}