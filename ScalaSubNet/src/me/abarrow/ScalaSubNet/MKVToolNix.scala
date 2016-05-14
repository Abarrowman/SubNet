package me.abarrow.ScalaSubNet

import scala.sys.process._
import scala.collection.mutable.ArrayBuffer
import java.io.File
import me.abarrow.ScalaSubNet.utils.FileUtils
import me.abarrow.ScalaSubNet.utils.StringUtils


class MKVToolNix(val mkvToolNixDir: String, val mkvInfoExecutable: String, val mkvExtractExecutable: String) {
  val trackNumberPrefix = "|  + Track number: "
  val trackTypePrefix = "|  + Track type: "
  val codecIdPrefix = "|  + Codec ID: "
  val trackNamePrefix = "|  + Language: "
  val trackLanguagePrefix = "|  + Name: "
  
  def determineSubTracks(targetFilePath: String): Seq[SubtitleTrack] = {    
    val command = Process(Seq(
        mkvToolNixDir + File.separatorChar + mkvInfoExecutable,
        targetFilePath
    ))
    //println(command)
    val result = command!!
    val lines = result.split(System.lineSeparator())
    var trackType: Option[String] = None
    var codecId: Option[String] = None
    var trackId: Option[Int] = None
    var trackName: Option[String] = None
    var trackLang: Option[String] = None
    var activeTrack = false
    var subtitleTracks = new ArrayBuffer[SubtitleTrack]()
    lines.foreach { x =>
      val startTrack = (x == "| + A track")
      if (activeTrack) {
        if (startTrack || x.startsWith("|+ EbmlVoid")) {
          if (trackType.isEmpty) {
            throw new RuntimeException("No track type supplied.")
          }
          if (codecId.isEmpty) {
            throw new RuntimeException("No codec id supplied.")
          }
          if (trackId.isEmpty) {
            throw new RuntimeException("No track id supplied.")
          }
          if (trackType.get == "subtitles") {
            subtitleTracks += new SubtitleTrack(trackId.get, codecId.get, trackName, trackLang)
          }
          trackType = None
          codecId = None
          trackId = None
          trackLang = None
          trackName = None

          if (!startTrack) {
            activeTrack = false
          }
        } else if (x.startsWith(trackNumberPrefix)) {
          trackId = Some(x.substring(trackNumberPrefix.length(), x.indexOf(" ", trackNumberPrefix.length())).toInt)
        } else if (x.startsWith(trackTypePrefix)) {
          trackType = Some(x.substring(trackTypePrefix.length()))
        } else if (x.startsWith(codecIdPrefix)) {
          codecId = Some(x.substring(codecIdPrefix.length()))
        } else if (x.startsWith(trackNamePrefix)) {
          trackName = Some(x.substring(trackNamePrefix.length()))
        } else if (x.startsWith(trackLanguagePrefix)) {
          trackLang = Some(x.substring(trackLanguagePrefix.length()))
        }
      } else if (startTrack) {
        activeTrack = true
      }
    }
    subtitleTracks
  }

  def determineBestSubTrackIdx(targetFilePath: String): Option[Int] = {
    val subtitleTracks = determineSubTracks(targetFilePath)
    val acceptableSubTracks = subtitleTracks.filter { x => (x.codecId == "S_TEXT/ASS") || (x.codecId == "S_TEXT/SSA") }
    var result: Option[Int] = None
    if (acceptableSubTracks.size != 0) {
      val ratedSubs = acceptableSubTracks.map { x =>
        var total = 0
        if (x.trackLang.isDefined) {
          val trackLangClean = x.trackLang.get.toLowerCase()
          val containsEng = trackLangClean.contains("eng")
          val containsJap = trackLangClean.contains("jap")
          if (containsEng) {
            total += 1
          }
          if (containsJap) {
            total += 2
          }
          if ((!containsEng) && (!containsJap)){
            total -= 10
          }
        }
        if (x.trackName.isDefined) {
          val trackNameClean = x.trackName.get.toLowerCase()
          if (trackNameClean.contains("eng")) {
            total += 1
          }
          if (trackNameClean.contains("jpn")) {
            total += 2
          }
          if (trackNameClean.contains("title")) {
            total = -1
          }
          if (trackNameClean.contains("sign")) {
            total = -1
          }
          if (trackNameClean.contains("song")) {
            total = -1
          }
        }
        (x, total)
      }
      val highEnouhgRatedSubs = ratedSubs.filter(p => p._2 >= 0)
      if (highEnouhgRatedSubs.size > 0) {
        val oneIdxedId = highEnouhgRatedSubs.maxBy(f => f._2)._1.trackId
        result = Some(oneIdxedId - 1) //Use 0-indexing as opposed to 1-indexing

      }
    }
    result
  }

  def extractTrack(srcFilePath: String, targetFilePath: String, trackIdx: Int): Unit = {
    val commandSeq = Seq(
        mkvToolNixDir + File.separatorChar + mkvExtractExecutable,
        "tracks",
        srcFilePath,
        trackIdx + ":" + targetFilePath
    )
    val command = Process(commandSeq)
    val result = command.!
    if (result != 0) {
      throw new RuntimeException("Extraction failed.")
    }
  }

  def extractBestSubTrack(srcFilePath: String): Option[String] = {
    var result:Option[String] = None
    val bestIdx:Option[Int] = determineBestSubTrackIdx(srcFilePath)
    if (bestIdx.isDefined) {
      val subFile = File.createTempFile(FileUtils.getFileNameAndExtension(srcFilePath)._1, ".ass")
      subFile.deleteOnExit();
      val subFilePath = subFile.getAbsolutePath()
      extractTrack(srcFilePath, subFilePath, bestIdx.get)
      result = Some(FileUtils.openUTF8Text(subFilePath))
      subFile.delete()
    }
    result
  }

}