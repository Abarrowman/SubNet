
package me.abarrow.ScalaSubNet

object ASSParser {
  val dialogFieldCount = 10
  val dialogueLine = "Dialogue: "

  def parseTimeStamp(timeStamp: String): Double = {
    val firstColon = timeStamp.indexOf(':')
    val lastColon = timeStamp.lastIndexOf(':')
    timeStamp.substring(0, firstColon).toDouble * 3600 +
      timeStamp.substring(firstColon + 1, lastColon).toDouble * 60 +
      timeStamp.substring(lastColon + 1).toDouble
  }

  def parse(assContent: String): String = {
    val lines = assContent.split(System.lineSeparator())
    val extractedLines = lines.map{ x =>
      var result: Option[(Double, String, String)] = None
      if (x.startsWith(dialogueLine)) {
        val dialogueFields = x.substring(dialogueLine.length()).split(",", dialogFieldCount)
        if (dialogueFields.size == dialogFieldCount) {
          val startTime = parseTimeStamp(dialogueFields(1))
          val style = dialogueFields(3)
          val text = dialogueFields(9)
          result = Some((startTime, style, text))
        }
      }
      result
    }.flatten
    extractedLines.sortBy(f => f._1).map { x =>
      val style = x._2.toLowerCase()
      if (style.startsWith("op") || style.startsWith("ed") || style.startsWith("end") || style.contains("song")) {
        None
      } else {
        Some(x._3.replaceAll("\\\\(n|N)", " ").replaceAll("\\{.*\\}", ""))
      }
    }.flatten.mkString("\n")
  }
}