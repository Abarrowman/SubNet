package me.abarrow.ScalaSubNet

import me.abarrow.ScalaSubNet.utils.FileUtils

object VideoUtils {
  def isMKVVideo(file: String): Boolean = {
    val unacceptableNames = "(^|[^a-z])(op|ed|oped|playall|pv)($|[^a-z])".r
    val nameExt = FileUtils.getFileNameAndExtension(file)
    nameExt._2.toLowerCase() == "mkv" && unacceptableNames.findFirstIn(nameExt._1.toLowerCase()).isEmpty
  }
}